{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LinkCheck
  ( linkCheck,
    runLinkCheck,
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
import Text.Printf
import UnliftIO

linkCheck :: IO ()
linkCheck = getSettings >>= runLinkCheck

runLinkCheck :: Settings -> IO ()
runLinkCheck Settings {..} = do
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
  fetchers <- case setFetchers of
    Nothing -> getNumCapabilities
    Just f -> pure f
  let indexes = [0 .. fetchers - 1]
  fetcherStati <- newTVarIO $ IM.fromList $ zip indexes (repeat True)
  atomically $ writeTQueue queue (setUri, 0)
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $ do
      logInfoN $ "Running with " <> T.pack (show fetchers) <> " fetchers"
      forConcurrently_ indexes $ \ix ->
        worker setExternal setCheckFragments setMaxDepth setUri man queue seen results fetcherStati fetchers ix
  resultsList <- M.toList <$> readTVarIO results
  unless (null resultsList) $
    die $
      unlines $
        map
          ( \(uri, result) ->
              unwords
                [ show uri,
                  prettyResult result
                ]
          )
          resultsList

data Result
  = ResultException HttpException
  | ResultStatus HTTP.Status
  | ResultFragmentMissing String
  deriving (Show)

prettyResult :: Result -> String
prettyResult = \case
  ResultException e -> displayException e
  ResultStatus status -> show status
  ResultFragmentMissing f -> "Fragment name or id not found: #" <> f

worker ::
  Bool ->
  Bool ->
  Maybe Word ->
  URI ->
  HTTP.Manager ->
  -- Queue
  TQueue (URI, Word) ->
  -- Seen
  TVar (Set URI) ->
  -- Results
  TVar (Map URI Result) ->
  TVar (IntMap Bool) ->
  Int ->
  Int ->
  LoggingT IO ()
worker fetchExternal checkFragments maxDepth root man queue seen results stati totalFetchers index = go True
  where
    fetcherName = case totalFetchers of
      1 -> "fetcher"
      _ ->
        let digits :: Int
            digits = ceiling (logBase 10 (fromIntegral totalFetchers) :: Double)
            formatStr = "%0" <> show digits <> "d"
         in T.pack $ "fetcher-" <> printf formatStr index
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
        Just (uri, curDepth) -> do
          -- Set this worker as busy
          -- logDebugN $ "Worker is busy: " <> T.pack (show index)
          unless busy setBusy
          if ((curDepth >) <$> maxDepth) == Just True
            then pure ()
            else do
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
                      logErrorNS fetcherName $ "Unable to construct a request from this uri: " <> T.pack (show uri)
                    Just req -> do
                      let fetchingLog = case maxDepth of
                            Nothing -> ["Fetching: ", show uri]
                            Just md -> ["Depth ", show curDepth, "/", show md, " Fetching: ", show uri]
                      logInfoNS fetcherName $ T.pack $ concat $ fetchingLog
                      -- Do the actual fetch
                      errOrResp <- liftIO $ retryHTTP req $ httpLbs req man
                      case errOrResp of
                        Left err -> do
                          logDebugNS fetcherName $ "Got exception for " <> T.pack (show uri) <> ": " <> T.pack (show err)
                          atomically $ modifyTVar' results $ M.insert uri (ResultException err)
                        Right resp -> do
                          let body = responseBody resp
                          let status = responseStatus resp
                          let sci = HTTP.statusCode status
                          logDebugNS fetcherName $ "Got response for " <> T.pack (show uri) <> ": " <> T.pack (show sci)
                          -- If the status code is not in the 2XX range, add it to the results
                          unless (200 <= sci && sci < 300) $ atomically $ modifyTVar' results $ M.insert uri (ResultStatus status)
                          -- Only recurse into the page if the page has the same root.
                          when (uriAuthority uri == uriAuthority root) $ do
                            -- Find all uris
                            let tags = parseTagsOptions parseOptionsFast body
                            when checkFragments $ do
                              case uriFragment uri of
                                "" -> pure ()
                                '#' : f -> do
                                  let fragmentLinkGood = LB.fromStrict (TE.encodeUtf8 (T.pack f)) `elem` concatMap tagIdOrName tags
                                  when (not fragmentLinkGood) $ atomically $ modifyTVar' results $ M.insert uri (ResultFragmentMissing (uriFragment uri))
                                _ -> pure ()

                            let removeFragment u = u {uriFragment = ""}
                            let uris =
                                  (if checkFragments then id else map removeFragment) $
                                    mapMaybe (parseURIRelativeTo uri) $
                                      mapMaybe (fmap T.unpack . rightToMaybe . TE.decodeUtf8' . LB.toStrict) $
                                        mapMaybe aTagHref tags
                            let predicate =
                                  if fetchExternal
                                    then const True
                                    else -- Filter out the ones that are not on the same host.
                                      (== uriAuthority root) . uriAuthority
                            let urisToAddToQueue = map (\u -> (u, succ curDepth)) $ filter predicate uris
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

tagIdOrName :: (Eq str, IsString str) => Tag str -> [str]
tagIdOrName = \case
  TagOpen _ as -> maybeToList (lookup "id" as) ++ maybeToList (lookup "name" as)
  _ -> []

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
