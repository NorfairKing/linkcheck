{-# LANGUAGE RecordWildCards #-}

module LinkCheck.OptParse
  ( module LinkCheck.OptParse,
    module LinkCheck.OptParse.Types,
  )
where

import Control.Monad.Logger
import Data.Maybe
import qualified Env
import LinkCheck.OptParse.Types
import Network.URI
import Options.Applicative
import qualified System.Environment as System
import Text.Read

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfig flags env
  deriveSettings flags env config

getConfig :: Flags -> Environment -> IO (Maybe Configuration)
getConfig _ _ = pure Nothing

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment _ = do
  let setUri = flagUri
      setLogLevel = fromMaybe LevelInfo flagLogLevel
      setFetchers = flagFetchers
  pure Settings {..}

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Flags
runArgumentsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = "",
          prefDisambiguate = True,
          prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True,
          prefBacktrack = True,
          prefColumns = 80
        }

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) fullDesc

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> argument
      (maybeReader parseAbsoluteURI)
      ( mconcat
          [ help "The root uri. This must be an absolute URI. For example: https://example.com or http://localhost:8000",
            metavar "URI"
          ]
      )
      <*> option
        (Just <$> maybeReader parseLogLevel)
        ( mconcat
            [ long "log-level",
              help $ "The log level, example values: " <> show (map (drop 5 . show) [LevelDebug, LevelInfo, LevelWarn, LevelError]),
              metavar "LOG_LEVEL",
              value Nothing
            ]
        )
      <*> option
        (Just <$> auto)
        ( mconcat
            [ long "fetchers",
              help "The number of threads to fetch from. This application is usually not CPU bound so you can comfortably set this higher than the number of cores you have",
              metavar "INT",
              value Nothing
            ]
        )

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser = pure Environment
