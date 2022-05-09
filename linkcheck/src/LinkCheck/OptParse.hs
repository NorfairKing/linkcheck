{-# LANGUAGE RecordWildCards #-}

module LinkCheck.OptParse
  ( module LinkCheck.OptParse,
    module LinkCheck.OptParse.Types,
  )
where

import Control.Monad.Logger
import Data.Maybe
import LinkCheck.OptParse.Types
import Network.URI
import Options.Applicative
import qualified System.Environment as System
import Text.Read

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  deriveSettings flags

deriveSettings :: Flags -> IO Settings
deriveSettings Flags {..} = do
  let setUri = flagUri
      setLogLevel = fromMaybe LevelInfo flagLogLevel
      setFetchers = flagFetchers
      setExternal = fromMaybe False flagExternal
      setCheckFragments = fromMaybe False flagCheckFragments
      setMaxDepth = flagMaxDepth
      setCacheSize = flagCacheSize
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
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
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
      <*> optional
        ( option
            auto
            ( mconcat
                [ long "fetchers",
                  help "The number of threads to fetch from. This application is usually not CPU bound so you can comfortably set this higher than the number of cores you have",
                  metavar "INT"
                ]
            )
        )
      <*> optional
        ( switch
            ( mconcat
                [ long "external",
                  help "Also check external links"
                ]
            )
        )
      <*> optional
        ( switch
            ( mconcat
                [ long "check-fragments",
                  help "Also check that the URIs' fragment occurs on the page"
                ]
            )
        )
      <*> optional
        ( option
            auto
            ( mconcat
                [ long "max-depth",
                  help "Stop looking after reaching this number of links from the root"
                ]
            )
        )
      <*> optional
        ( option
            auto
            ( mconcat
                [ long "cache-size",
                  help "Cache this many requests' fragments."
                ]
            )
        )

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s
