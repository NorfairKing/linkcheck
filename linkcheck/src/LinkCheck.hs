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
        worker
          WorkerSettings
            { workerSetExternal = setExternal,
              workerSetCheckFragments = setCheckFragments,
              workerSetMaxDepth = setMaxDepth,
              workerSetRootURI = setUri,
              workerSetHTTPManager = man,
              workerSetURIQueue = queue,
              workerSetSeenSet = seen,
              workerSetResultsMap = results,
              workerSetStatusMap = fetcherStati,
              workerSetTotalFetchers = fetchers,
              workerSetWorkerIndex = ix
            }
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

data WorkerSettings = WorkerSettings
  { workerSetExternal :: !Bool,
    workerSetCheckFragments :: !Bool,
    workerSetMaxDepth :: !(Maybe Word),
    workerSetRootURI :: !URI,
    workerSetHTTPManager :: !HTTP.Manager,
    workerSetURIQueue :: !(TQueue (URI, Word)),
    workerSetSeenSet :: !(TVar (Set URI)),
    workerSetResultsMap :: !(TVar (Map URI Result)),
    workerSetStatusMap :: !(TVar (IntMap Bool)),
    workerSetTotalFetchers :: !Int,
    workerSetWorkerIndex :: !Int
  }

worker ::
  WorkerSettings ->
  LoggingT IO ()
worker WorkerSettings {..} = go True
  where
    fetcherName = case workerSetTotalFetchers of
      1 -> "fetcher"
      _ ->
        let digits :: Int
            digits = ceiling (logBase 10 (fromIntegral workerSetTotalFetchers) :: Double)
            formatStr = "%0" <> show digits <> "d"
         in T.pack $ "fetcher-" <> printf formatStr workerSetWorkerIndex
    setStatus b = atomically $ modifyTVar' workerSetStatusMap $ IM.insert workerSetWorkerIndex b
    setBusy = setStatus True
    setIdle = setStatus False
    allDone :: MonadIO m => m Bool
    allDone = all not <$> readTVarIO workerSetStatusMap
    go busy = do
      mv <- atomically $ tryReadTQueue workerSetURIQueue
      -- Get an item off the queue
      case mv of
        -- No items on the queue
        Nothing -> do
          -- Set this worker as idle
          logDebugN $ "Worker is idle: " <> T.pack (show workerSetWorkerIndex)
          when busy setIdle
          -- If all workers are idle, we are done.
          ad <- allDone
          unless ad $ do
            liftIO $ threadDelay 10000 -- 10 ms
            go False
        -- An item on the queue
        Just (uri, curDepth) -> do
          -- Set this worker as busy
          logDebugN $ "Worker is busy: " <> T.pack (show workerSetWorkerIndex)
          unless busy setBusy
          if ((curDepth >) <$> workerSetMaxDepth) == Just True
            then pure ()
            else do
              -- Check if the uri has been seen already
              alreadySeen <- atomically $ S.member uri <$> readTVar workerSetSeenSet
              if alreadySeen
                then do
                  -- We've already seen it, don't do anything.
                  logDebugN $ "Not fetching again: " <> T.pack (show uri)
                  pure ()
                else do
                  -- We haven't seen it yet. Mark it as seen.
                  atomically $ modifyTVar' workerSetSeenSet $ S.insert uri
                  -- Create a request
                  case requestFromURI uri of
                    Nothing ->
                      logErrorNS fetcherName $ "Unable to construct a request from this uri: " <> T.pack (show uri)
                    Just req -> do
                      let fetchingLog = case workerSetMaxDepth of
                            Nothing -> ["Fetching: ", show uri]
                            Just md -> ["Depth ", show curDepth, "/", show md, " Fetching: ", show uri]
                      logInfoNS fetcherName $ T.pack $ concat $ fetchingLog
                      -- Do the actual fetch
                      errOrResp <- liftIO $ retryHTTP req $ httpLbs req workerSetHTTPManager
                      case errOrResp of
                        Left err -> do
                          logDebugNS fetcherName $ "Got exception for " <> T.pack (show uri) <> ": " <> T.pack (show err)
                          atomically $ modifyTVar' workerSetResultsMap $ M.insert uri (ResultException err)
                        Right resp -> do
                          let body = responseBody resp
                          let status = responseStatus resp
                          let sci = HTTP.statusCode status
                          logDebugNS fetcherName $ "Got response for " <> T.pack (show uri) <> ": " <> T.pack (show sci)
                          -- If the status code is not in the 2XX range, add it to the results
                          unless (200 <= sci && sci < 300) $ atomically $ modifyTVar' workerSetResultsMap $ M.insert uri (ResultStatus status)
                          -- Only recurse into the page if the page has the same root.
                          when (uriAuthority uri == uriAuthority workerSetRootURI) $ do
                            -- Find all uris
                            let tags = parseTagsOptions parseOptionsFast body
                            when workerSetCheckFragments $ do
                              case uriFragment uri of
                                "" -> pure ()
                                '#' : f -> do
                                  let fragmentLinkGood = LB.fromStrict (TE.encodeUtf8 (T.pack f)) `elem` concatMap tagIdOrName tags
                                  when (not fragmentLinkGood) $ atomically $ modifyTVar' workerSetResultsMap $ M.insert uri (ResultFragmentMissing (uriFragment uri))
                                _ -> pure ()

                            let removeFragment u = u {uriFragment = ""}
                            let uris =
                                  (if workerSetCheckFragments then id else map removeFragment) $
                                    mapMaybe (parseURIRelativeTo uri) $
                                      mapMaybe (fmap T.unpack . rightToMaybe . TE.decodeUtf8' . LB.toStrict) $
                                        mapMaybe aTagHref tags
                            let predicate =
                                  if workerSetExternal
                                    then const True
                                    else -- Filter out the ones that are not on the same host.
                                      (== uriAuthority workerSetRootURI) . uriAuthority
                            let urisToAddToQueue = map (\u -> (u, succ curDepth)) $ filter predicate uris
                            atomically $ mapM_ (writeTQueue workerSetURIQueue) urisToAddToQueue
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
