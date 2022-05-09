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
import Control.Concurrent.STM (stateTVar)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Cache.LRU (LRU, newLRU)
import qualified Data.Cache.LRU as LRU
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple
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
  cache <- newTVarIO $ newLRU Nothing -- TODO make it possible to configure bounds
  results <- newTVarIO M.empty
  fetchers <- case setFetchers of
    Nothing -> getNumCapabilities
    Just f -> pure f
  let indexes = [0 .. fetchers - 1]
  fetcherStati <- newTVarIO $ IM.fromList $ zip indexes (repeat True)
  atomically $ writeTQueue queue QueueURI {queueURI = setUri, queueURIDepth = 0, queueURITrace = []}
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $ do
      logInfoN $
        T.pack $
          unwords
            [ "Running with",
              show fetchers,
              "fetchers"
            ]
      forConcurrently_ indexes $ \ix ->
        worker
          WorkerSettings
            { workerSetExternal = setExternal,
              workerSetCheckFragments = setCheckFragments,
              workerSetMaxDepth = setMaxDepth,
              workerSetRoot = setUri,
              workerSetHTTPManager = man,
              workerSetURIQueue = queue,
              workerSetSeenSet = seen,
              workerSetCache = cache,
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

data Result = Result
  { resultReason :: !ResultReason,
    resultTrace :: ![URI]
  }
  deriving (Show)

data ResultReason
  = ResultReasonException HttpException
  | ResultReasonStatus HTTP.Status
  | ResultReasonFragmentMissing String
  deriving (Show)

prettyResult :: Result -> String
prettyResult Result {..} = do
  unlines
    ( unwords ["Reason:", prettyResultReason resultReason] :
      "Trace:" :
      map show resultTrace
    )

prettyResultReason :: ResultReason -> String
prettyResultReason = \case
  ResultReasonException e -> displayException e
  ResultReasonStatus status -> show status
  ResultReasonFragmentMissing f -> "Fragment name or id not found: #" <> f

data WorkerSettings = WorkerSettings
  { workerSetExternal :: !Bool,
    workerSetCheckFragments :: !Bool,
    workerSetMaxDepth :: !(Maybe Word),
    workerSetRoot :: !URI,
    workerSetHTTPManager :: !HTTP.Manager,
    workerSetURIQueue :: !(TQueue QueueURI),
    workerSetSeenSet :: !(TVar (Set URI)),
    workerSetCache :: !(TVar (LRU URI [Tag SB.ByteString])),
    workerSetResultsMap :: !(TVar (Map URI Result)),
    workerSetStatusMap :: !(TVar (IntMap Bool)),
    workerSetTotalFetchers :: !Int,
    workerSetWorkerIndex :: !Int
  }

data QueueURI = QueueURI
  { queueURI :: !URI,
    queueURIDepth :: !Word,
    queueURITrace :: ![URI]
  }

worker ::
  WorkerSettings ->
  LoggingT IO ()
worker WorkerSettings {..} = addFetcherNameToLog fetcherName $ go True
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
          logDebugN $
            T.pack $
              unwords
                [ "Worker is idle:",
                  show workerSetWorkerIndex
                ]
          when busy setIdle
          -- If all workers are idle, we are done.
          ad <- allDone
          unless ad $ do
            liftIO $ threadDelay 10000 -- 10 ms
            go False
        -- An item on the queue
        Just QueueURI {..} -> do
          -- Set this worker as busy
          logDebugN $
            T.pack $
              unwords
                [ "Worker is busy:",
                  show workerSetWorkerIndex
                ]
          unless busy setBusy
          -- Check if the uri has been seen already
          alreadySeen <- atomically $ S.member queueURI <$> readTVar workerSetSeenSet
          if alreadySeen
            then do
              -- We've already seen it, don't do anything.
              logDebugN $
                T.pack $
                  unwords
                    [ "Not fetching again:",
                      show queueURI
                    ]
            else do
              -- We haven't seen it yet. Mark it as seen.
              atomically $ modifyTVar' workerSetSeenSet $ S.insert queueURI
              -- Check if the response is cached
              let cacheURI = queueURI {uriFragment = ""}
              mCachedResult <- atomically $ stateTVar workerSetCache $ swap . LRU.lookup cacheURI
              let insertResult reason =
                    atomically $
                      modifyTVar' workerSetResultsMap $
                        M.insert
                          queueURI
                          Result
                            { resultReason = reason,
                              resultTrace = queueURITrace
                            }
              -- Check if we have the fragments cached
              mResp <- case mCachedResult of
                -- Found in cache
                Just cachedResponse -> do
                  logInfoN $
                    T.pack $
                      unwords
                        [ "Not fetching because the page is already cached:",
                          show queueURI
                        ]
                  pure $ Just cachedResponse
                -- Not found in cache
                Nothing -> do
                  -- Create a request
                  case requestFromURI queueURI of
                    -- Making the request failed
                    Nothing -> do
                      logErrorN $
                        T.pack $
                          unwords
                            [ "Unable to construct a request from this uri:",
                              show queueURI
                            ]
                      pure Nothing
                    -- Making the request succeeded
                    Just req -> do
                      -- Do the actual fetch
                      let fetchingLog = case workerSetMaxDepth of
                            Nothing ->
                              [ "Fetching: ",
                                show queueURI
                              ]
                            Just md ->
                              [ "Depth ",
                                show queueURIDepth,
                                "/",
                                show md,
                                "; Fetching: ",
                                show queueURI
                              ]
                      logInfoN $ T.pack $ concat fetchingLog
                      errOrResp <- liftIO $ retryHTTP req $ httpLbs req workerSetHTTPManager
                      case errOrResp of
                        -- Something went wrong during the fetch.
                        Left err -> do
                          logDebugN $
                            T.pack $
                              unwords
                                [ "Got exception for",
                                  show queueURI <> ":",
                                  show err
                                ]
                          insertResult $ ResultReasonException err
                          pure Nothing

                        -- Got a response
                        Right resp -> do
                          let status = responseStatus resp
                          let sci = HTTP.statusCode status
                          logDebugN $
                            T.pack $
                              unwords
                                [ "Got response for",
                                  show queueURI <> ": ",
                                  show sci
                                ]
                          -- If the status code is not in the 2XX range, add it to the results
                          unless (200 <= sci && sci < 300) $ insertResult $ ResultReasonStatus status

                          -- Read the entire response and parse tags
                          let body = LB.toStrict $ responseBody resp
                          let tags = parseTagsOptions parseOptionsFast body

                          -- Only recurse into the page if we're not deep enough already
                          let shouldRecurseByDepth = case workerSetMaxDepth of
                                Nothing -> True
                                Just md -> queueURIDepth < md

                          -- Only recurse into the page if the page has the same root.
                          let shouldRecurseByAuthority = uriAuthority queueURI == uriAuthority workerSetRoot

                          let shouldRecurse = shouldRecurseByDepth && shouldRecurseByAuthority

                          when shouldRecurse $ do
                            let removeFragment u = u {uriFragment = ""}
                            let uris =
                                  (if workerSetCheckFragments then id else map removeFragment) $
                                    mapMaybe (parseURIRelativeTo queueURI) $
                                      mapMaybe (fmap T.unpack . rightToMaybe . TE.decodeUtf8') $
                                        mapMaybe aTagHref tags

                            let predicate =
                                  if workerSetExternal
                                    then const True
                                    else -- Filter out the ones that are not on the same host.
                                      (== uriAuthority workerSetRoot) . uriAuthority
                            let urisToAddToQueue =
                                  map
                                    ( \u ->
                                        QueueURI
                                          { queueURI = u,
                                            queueURIDepth = succ queueURIDepth,
                                            queueURITrace = queueURI : queueURITrace
                                          }
                                    )
                                    $ filter predicate uris
                            atomically $ mapM_ (writeTQueue workerSetURIQueue) urisToAddToQueue

                          -- Insert the fragments into the cache.
                          atomically $ modifyTVar' workerSetCache $ LRU.insert cacheURI tags
                          pure $ Just tags

              case mResp of
                Nothing -> pure ()
                Just tags -> do
                  -- Check that the fragments are in order.
                  when workerSetCheckFragments $ do
                    case uriFragment queueURI of
                      "" -> pure ()
                      '#' : f -> do
                        let fragmentLinkGood = TE.encodeUtf8 (T.pack f) `elem` concatMap tagIdOrName tags
                        when (not fragmentLinkGood) $ insertResult $ ResultReasonFragmentMissing (uriFragment queueURI)
                      _ -> pure ()
          go True

addFetcherNameToLog :: Text -> LoggingT m a -> LoggingT m a
addFetcherNameToLog fetcherName = modLogSource $ \source -> if source == "" then fetcherName else source

modLogSource :: (LogSource -> LogSource) -> LoggingT m a -> LoggingT m a
modLogSource func (LoggingT mFunc) = LoggingT $ \logFunc ->
  let newLogFunc loc source level str =
        let source' = func source
         in logFunc loc source' level str
   in mFunc newLogFunc

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
