{-# LANGUAGE BangPatterns #-}
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
import Control.DeepSeq (force)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.Cache.LRU (LRU, newLRU)
import qualified Data.Cache.LRU as LRU
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple
import Data.Validity.URI
import Data.Version
import LinkCheck.OptParse
import qualified ListT
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (getRedirectedRequest, httpRaw, httpRedirect)
import Network.HTTP.Client.Internal as HTTP (toHttpException)
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import Paths_linkcheck
import qualified StmContainers.Map as StmMap
import qualified StmContainers.Set as StmSet
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
                    )
                      : requestHeaders request
              pure $ request {requestHeaders = headers}
          }
  man <- liftIO $ HTTP.newManager managerSets
  queue <- newTQueueIO
  seen <- StmSet.newIO
  mCache <-
    if setCheckFragments
      then
        fmap Just $
          newTVarIO $
            newLRU $
              fromIntegral <$> setCacheSize
      else pure Nothing -- no need to cache anything if we don't check fragments anyway.
  results <- StmMap.newIO
  fetchers <- case setFetchers of
    Nothing -> getNumCapabilities
    Just f -> pure f
  let indexes = [0 .. fetchers - 1]
  busyCount <- newTVarIO fetchers
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
              workerSetCache = mCache,
              workerSetResultsMap = results,
              workerSetBusyCount = busyCount,
              workerSetTotalFetchers = fetchers,
              workerSetWorkerIndex = ix
            }
  resultsList <- atomically $ ListT.toList $ StmMap.listT results
  unless (null resultsList) $
    die $
      unlines $
        map
          ( \(uri, result) ->
              unwords
                [ T.unpack uri,
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
    ( unwords ["Reason:", prettyResultReason resultReason]
        : "Trace:"
        : map show resultTrace
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
    workerSetSeenSet :: !(StmSet.Set Text),
    workerSetCache :: !(Maybe (TVar (LRU URI [SB.ByteString]))),
    workerSetResultsMap :: !(StmMap.Map Text Result),
    workerSetBusyCount :: !(TVar Int),
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
    setBusy = atomically $ modifyTVar' workerSetBusyCount (+ 1)
    setIdle = atomically $ modifyTVar' workerSetBusyCount (subtract 1)
    allDone :: (MonadIO m) => m Bool
    allDone = (== 0) <$> readTVarIO workerSetBusyCount
    go busy = do
      mv <-
        if busy
          then atomically $ tryReadTQueue workerSetURIQueue
          else timeout 100_000 (atomically $ readTQueue workerSetURIQueue)
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
          unless ad $ go False
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
          let queueURIText = T.pack $ dangerousURIToString queueURI
          alreadySeen <- atomically $ StmSet.lookup queueURIText workerSetSeenSet
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
              atomically $ StmSet.insert queueURIText workerSetSeenSet

              -- Helper function for inserting a result.
              -- We'll need this in both the cached and uncached branches below
              -- so we'll already define it here.
              let insertResult reason =
                    atomically $
                      StmMap.insert
                        Result
                          { resultReason = reason,
                            resultTrace = queueURITrace
                          }
                        queueURIText
                        workerSetResultsMap

              -- Check if the response is cached
              let cacheURI = queueURI {uriFragment = ""}
              mCachedResult <- case workerSetCache of
                Nothing -> pure Nothing -- Can't be cached if there is no cache
                Just cache -> atomically $ stateTVar cache $ swap . LRU.lookup cacheURI

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
                      let externalPredicate =
                            if workerSetExternal
                              then const True
                              else -- Filter out the ones that are not on the same host.
                                (== uriAuthority workerSetRoot) . uriAuthority
                      errOrResp <- liftIO $ retryHTTP req $ httpWithRedirects externalPredicate req workerSetHTTPManager
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
                          -- If the status code is not in the 2XX or 3XX ranges, add it to the results
                          -- 300 means either "too many redirects" or "the redirect is to somewhere external".
                          unless (200 <= sci && sci < 400) $ insertResult $ ResultReasonStatus status

                          -- Read the entire response and parse tags
                          let !body = LB.toStrict $ responseBody resp
                          let !tags = parseTagsOptions parseOptionsFast body

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
                                    concatMap
                                      ( mapMaybe (parseURIRelativeTo queueURI)
                                          . mapMaybe (fmap T.unpack . rightToMaybe . TE.decodeUtf8')
                                          . tagURLs
                                      )
                                      tags

                            let urisToAddToQueue =
                                  map
                                    ( \u ->
                                        QueueURI
                                          { queueURI = u,
                                            queueURIDepth = succ queueURIDepth,
                                            queueURITrace = queueURI : queueURITrace
                                          }
                                    )
                                    $ filter externalPredicate uris
                            atomically $ mapM_ (writeTQueue workerSetURIQueue) urisToAddToQueue

                          -- Compute the fragments
                          let !fragments = force $ concatMap tagIdOrName tags

                          -- Insert the fragments into the cache.
                          forM_ workerSetCache $ \cache ->
                            atomically $ modifyTVar' cache $ LRU.insert cacheURI fragments

                          pure $ Just fragments

              case mResp of
                Nothing -> pure ()
                Just fragments -> do
                  -- Check that the fragments are in order.
                  when workerSetCheckFragments $ do
                    case uriFragment queueURI of
                      "" -> pure ()
                      '#' : f -> do
                        let fragmentLinkGood = TE.encodeUtf8 (T.pack f) `elem` fragments
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

tagURLs :: Tag SB.ByteString -> [SB.ByteString]
tagURLs = \case
  TagOpen "a" as -> maybeToList $ lookup "href" as
  TagOpen "link" as -> maybeToList $ lookup "href" as
  TagOpen "img" as -> maybeToList (lookup "src" as) ++ parseSrcset (lookup "srcset" as)
  TagOpen "script" as -> maybeToList $ lookup "src" as
  TagOpen "iframe" as -> maybeToList $ lookup "src" as
  TagOpen "video" as -> maybeToList $ lookup "src" as `mplus` lookup "poster" as
  TagOpen "audio" as -> maybeToList $ lookup "src" as
  TagOpen "source" as -> maybeToList (lookup "src" as) ++ parseSrcset (lookup "srcset" as)
  TagOpen "embed" as -> maybeToList $ lookup "src" as
  TagOpen "object" as -> maybeToList $ lookup "data" as
  TagOpen "form" as -> maybeToList $ lookup "action" as
  TagOpen "meta" as -> maybeToList $ metaImageContent as
  _ -> []

-- | Parse a srcset attribute value into individual URLs.
-- The srcset format is: "url1 descriptor1, url2 descriptor2, ..."
parseSrcset :: Maybe SB.ByteString -> [SB.ByteString]
parseSrcset Nothing = []
parseSrcset (Just s) =
  mapMaybe extractURL $ SB8.split ',' s
  where
    extractURL entry = case SB8.words entry of
      [] -> Nothing
      (url : _) -> Just url

-- | Extract the content attribute from meta tags that reference images.
-- Covers og:image, twitter:image, and similar.
metaImageContent :: (Eq str, IsString str) => [(str, str)] -> Maybe str
metaImageContent as = do
  let prop = lookup "property" as
      name = lookup "name" as
  guard $ prop == Just "og:image" || name == Just "twitter:image"
  lookup "content" as

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

httpWithRedirects :: (URI -> Bool) -> Request -> HTTP.Manager -> IO (Response LB.ByteString)
httpWithRedirects externalPredicate request man = httpRedirect 10 go request >>= consumeBody
  where
    go :: HTTP.Request -> IO (Response HTTP.BodyReader, Maybe HTTP.Request)
    go r = do
      response <- httpRaw r man
      pure
        ( response,
          do
            newReq <-
              getRedirectedRequest
                request
                r
                (responseHeaders response)
                (responseCookieJar response)
                (statusCode (responseStatus response))
            guard $ externalPredicate (getUri newReq)
            pure newReq
        )

    consumeBody res = do
      bss <- brConsume $ responseBody res
      return res {responseBody = LB.fromChunks bss}
