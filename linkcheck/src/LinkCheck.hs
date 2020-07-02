{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LinkCheck
  ( linkCheck,
  )
where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import LinkCheck.OptParse
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import System.Exit
import Text.HTML.TagSoup

linkCheck :: IO ()
linkCheck = do
  Settings {..} <- getSettings
  man <- HTTP.newTlsManager
  queue <- newTQueueIO :: IO (TQueue URI)
  seen <- newTVarIO S.empty :: IO (TVar (Set URI))
  results <- newTVarIO M.empty :: IO (TVar (Map URI HTTP.Status))
  let go = do
        mv <- atomically $ tryReadTQueue queue
        case mv of
          Nothing -> pure () -- Done
          Just uri -> do
            alreadySeen <- (S.member uri) <$> readTVarIO seen
            if alreadySeen
              then go
              else do
                atomically $ modifyTVar' seen $ S.insert uri
                case requestFromURI uri of
                  Nothing -> die $ "Invalid uri: " <> show setUri
                  Just req -> do
                    body <- withResponse req man $ \resp -> do
                      bodyChunks <- brConsume $ responseBody resp
                      let status = responseStatus resp
                      let sci = HTTP.statusCode status
                      unless (200 <= sci && sci < 300) $ atomically $ modifyTVar' results $ M.insert uri status
                      pure $ LB.fromChunks bodyChunks
                    let tags = parseTagsOptions parseOptionsFast body
                    let uris = mapMaybe (parseURIRelativeTo setUri) $ mapMaybe (fmap T.unpack . rightToMaybe . TE.decodeUtf8' . LB.toStrict) $ mapMaybe aTagHref tags :: [URI]
                    let allSameHostAbsoluteUris = filter ((== uriAuthority setUri) . uriAuthority) uris
                    atomically $ mapM_ (writeTQueue queue) allSameHostAbsoluteUris
                    go
  atomically $ writeTQueue queue setUri
  go
  resultsList <- M.toList <$> readTVarIO results
  unless (null resultsList)
    $ die
    $ unlines
    $ map (\(uri, status) -> unwords [show uri, show status])
    $ resultsList

parseURIRelativeTo :: URI -> String -> Maybe URI
parseURIRelativeTo root s =
  msum
    [ (`relativeTo` root) <$> parseRelativeReference s,
      parseAbsoluteURI s
    ]

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = \case
  Left _ -> Nothing
  Right a -> Just a

aTagHref :: (Eq str, IsString str) => Tag str -> Maybe str
aTagHref = \case
  TagOpen "a" as -> lookup "href" as
  _ -> Nothing
