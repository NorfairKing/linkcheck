{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module LinkCheck.OptParse
  ( getSettings,
    Settings (..),
  )
where

import Control.Monad.Logger
import Network.URI
import OptEnvConf
import Paths_linkcheck (version)
import Text.Read

getSettings :: IO Settings
getSettings = runSettingsParser version "Check links on a website"

data Settings = Settings
  { setUri :: !URI,
    setLogLevel :: !LogLevel,
    setFetchers :: !(Maybe Int),
    setExternal :: !Bool,
    setCheckFragments :: !Bool,
    setMaxDepth :: !(Maybe Word),
    setCacheSize :: !(Maybe Word)
  }
  deriving (Show, Eq)

instance HasParser Settings where
  settingsParser = do
    setUri <-
      setting
        [ help "The root uri. This must be an absolute URI.",
          reader $ maybeReader parseAbsoluteURI,
          argument,
          metavar "URI",
          example "https://example.com",
          example "http://localhost:8000"
        ]
    setLogLevel <-
      setting
        [ help "Minimal severity of log messages",
          reader $ maybeReader parseLogLevel,
          option,
          long "log-level",
          metavar "LOG_LEVEL",
          value LevelInfo
        ]
    setFetchers <-
      optional $
        setting
          [ help "The number of threads to fetch from. This application is usually not CPU bound so you can comfortably set this higher than the number of cores you have",
            reader auto,
            option,
            long "fetchers",
            metavar "NUM"
          ]
    setExternal <-
      setting
        [ help "Also check external links",
          switch True,
          long "external",
          value False
        ]
    setCheckFragments <-
      setting
        [ help "Also check that the URIs' fragment occurs on the page",
          switch True,
          long "check-fragments",
          value False
        ]
    setMaxDepth <-
      optional $
        setting
          [ help "Stop looking after reaching this number of links from the root",
            reader auto,
            option,
            long "max-depth",
            metavar "NUM"
          ]
    setCacheSize <-
      optional $
        setting
          [ help "Cache this many requests' fragments.",
            reader auto,
            option,
            long "cache-size",
            metavar "NUM"
          ]

    pure Settings {..}

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s
