{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LinkCheck
  ( linkCheck,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
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
import UnliftIO

linkCheck :: IO ()
linkCheck = do
  Settings {..} <- getSettings
  man <- HTTP.newTlsManager
  queue <- newTQueueIO
  seen <- newTVarIO S.empty
  results <- newTVarIO M.empty
  atomically $ writeTQueue queue setUri
  runStderrLoggingT $ filterLogger (\_ ll -> ll >= setLogLevel) $ worker setUri man queue seen results
  resultsList <- M.toList <$> readTVarIO results
  unless (null resultsList)
    $ die
    $ unlines
    $ map (\(uri, status) -> unwords [show uri, show status])
    $ resultsList

worker :: URI -> HTTP.Manager -> TQueue URI -> TVar (Set URI) -> TVar (Map URI HTTP.Status) -> LoggingT IO ()
worker root man queue seen results = go
  where
    go = do
      mv <- atomically $ tryReadTQueue queue
      case mv of
        Nothing -> pure () -- Done
        Just uri -> do
          alreadySeen <- (S.member uri) <$> readTVarIO seen
          if alreadySeen
            then do
              logDebugN $ "Not fetching again: " <> T.pack (show uri)
              go
            else do
              atomically $ modifyTVar' seen $ S.insert uri
              case requestFromURI uri of
                Nothing -> do
                  logErrorN $ "Unable to construct a request from this uri: " <> T.pack (show uri)
                  pure () -- Just ignore
                Just req -> do
                  logInfoN $ "Fetching: " <> T.pack (show uri)
                  resp <- liftIO $ httpLbs req man
                  let body = responseBody resp
                  let status = responseStatus resp
                  let sci = HTTP.statusCode status
                  logDebugN $ "Got response for " <> T.pack (show uri) <> ": " <> T.pack (show sci)
                  unless (200 <= sci && sci < 300) $ atomically $ modifyTVar' results $ M.insert uri status
                  let tags = parseTagsOptions parseOptionsFast body
                  let uris = mapMaybe (parseURIRelativeTo root) $ mapMaybe (fmap T.unpack . rightToMaybe . TE.decodeUtf8' . LB.toStrict) $ mapMaybe aTagHref tags :: [URI]
                  let allSameHostAbsoluteUris = filter ((== uriAuthority root) . uriAuthority) uris
                  atomically $ mapM_ (writeTQueue queue) allSameHostAbsoluteUris
                  go

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
