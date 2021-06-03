{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LinkCheck
  ( linkCheck,
  )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import qualified Data.ByteString.Lazy as LB
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import LinkCheck.OptParse
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal as HTTP (toHttpException)
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import Paths_linkcheck
import System.Exit
import Text.HTML.TagSoup
import UnliftIO

linkCheck :: IO ()
linkCheck = do
  Settings {..} <- getSettings
  let managerSets =
        HTTP.tlsManagerSettings
          { managerModifyRequest = \request -> do
              let headers =
                    ( "User-Agent",
                      TE.encodeUtf8 $ T.pack $ "linkcheck-" <> showVersion version
                    ) :
                    requestHeaders request
              pure $ request {requestHeaders = headers}
          }
  man <- liftIO $ HTTP.newManager managerSets
  queue <- newTQueueIO
  seen <- newTVarIO S.empty
  results <- newTVarIO M.empty
  let fetchers = fromMaybe 1 setFetchers
      indexes = [0 .. fetchers - 1]
  fetcherStati <- newTVarIO $ IM.fromList $ zip indexes (repeat True)
  atomically $ writeTQueue queue setUri
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $ do
      logInfoN $ "Running with " <> T.pack (show fetchers) <> " fetchers"
      forConcurrently_ indexes $ \ix ->
        worker setExternal setUri man queue seen results fetcherStati ix
  resultsList <- M.toList <$> readTVarIO results
  unless (null resultsList) $
    die $
      unlines $
        map
          ( \(uri, errOrStatus) ->
              unwords
                [ show uri,
                  case errOrStatus of
                    Left err -> show err
                    Right status -> show status
                ]
          )
          resultsList

worker ::
  Bool ->
  URI ->
  HTTP.Manager ->
  -- Queue
  TQueue URI ->
  -- Seen
  TVar (Set URI) ->
  -- Results
  TVar (Map URI (Either HttpException HTTP.Status)) ->
  TVar (IntMap Bool) ->
  Int ->
  LoggingT IO ()
worker fetchExternal root man queue seen results stati index = go True
  where
    setStatus b = atomically $ modifyTVar' stati $ IM.insert index b
    setBusy = setStatus True
    setIdle = setStatus False
    allDone :: MonadIO m => m Bool
    allDone = all not <$> readTVarIO stati
    go busy = do
      mv <- atomically $ tryReadTQueue queue
      -- Get an item off the queue
      case mv of
        -- No items on the queue
        Nothing -> do
          -- Set this worker as idle
          -- logDebugN $ "Worker is idle: " <> T.pack (show index)
          when busy setIdle
          -- If all workers are idle, we are done.
          ad <- allDone
          unless ad $ do
            liftIO $ threadDelay 10000 -- 10 ms
            go False
        -- An item on the queue
        Just uri -> do
          -- Set this worker as busy
          -- logDebugN $ "Worker is busy: " <> T.pack (show index)
          unless busy setBusy
          -- Check if the uri has been seen already
          alreadySeen <- atomically $ S.member uri <$> readTVar seen
          if alreadySeen
            then -- We've already seen it, don't do anything.
            -- logDebugN $ "Not fetching again: " <> T.pack (show uri)
              pure ()
            else do
              -- We haven't seen it yet. Mark it as seen.
              atomically $ modifyTVar' seen $ S.insert uri
              -- Create a request
              case requestFromURI uri of
                Nothing ->
                  logErrorN $ "Unable to construct a request from this uri: " <> T.pack (show uri)
                Just req -> do
                  logInfoN $ "Fetching: " <> T.pack (show uri)
                  -- Do the actual fetch
                  errOrResp <- liftIO $ retryHTTP req $ httpLbs req man
                  case errOrResp of
                    Left err -> do
                      logDebugN $ "Got exception for " <> T.pack (show uri) <> ": " <> T.pack (show err)
                      atomically $ modifyTVar' results $ M.insert uri (Left err)
                    Right resp -> do
                      let body = responseBody resp
                      let status = responseStatus resp
                      let sci = HTTP.statusCode status
                      logDebugN $ "Got response for " <> T.pack (show uri) <> ": " <> T.pack (show sci)
                      -- If the status code is not in the 2XX range, add it to the results
                      unless (200 <= sci && sci < 300) $ atomically $ modifyTVar' results $ M.insert uri (Right status)
                      -- Only recurse into the page if the page has the same root.
                      when (uriAuthority uri == uriAuthority root) $ do
                        -- Find all uris
                        let tags = parseTagsOptions parseOptionsFast body
                        let removeFragment u = u {uriFragment = ""}
                        let uris =
                              map removeFragment $
                                mapMaybe (parseURIRelativeTo root) $
                                  mapMaybe (fmap T.unpack . rightToMaybe . TE.decodeUtf8' . LB.toStrict) $
                                    mapMaybe aTagHref tags
                        let predicate =
                              if fetchExternal
                                then const True
                                else -- Filter out the ones that are not on the same host.
                                  (== uriAuthority root) . uriAuthority
                        let urisToAddToQueue = filter predicate uris
                        atomically $ mapM_ (writeTQueue queue) urisToAddToQueue
          -- Filter out the ones that are not on the same host.
          go True

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
  TagOpen "link" as -> lookup "href" as
  TagOpen "img" as -> lookup "src" as
  _ -> Nothing

retryHTTP ::
  -- | Just  for the error message
  Request ->
  IO (Response a) ->
  IO (Either HttpException (Response a))
retryHTTP req action =
  let policy =
        mconcat
          [ exponentialBackoff 100_000,
            limitRetries 3
          ]
   in retrying
        policy
        (\_ e -> pure (couldBeFlaky e))
        ( \_ ->
            (Right <$> action)
              `catches` [ Handler $ pure . Left,
                          Handler $ pure . Left . toHttpException req
                        ]
        )
  where
    couldBeFlaky (Left e) = case e of
      HttpExceptionRequest _ hec -> case hec of
        ResponseTimeout -> True
        ConnectionTimeout -> True
        ConnectionFailure _ -> True
        NoResponseDataReceived -> True
        _ -> False
      InvalidUrlException _ _ -> False
    couldBeFlaky _ = False
