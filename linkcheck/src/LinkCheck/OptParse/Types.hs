module LinkCheck.OptParse.Types where

import Control.Monad.Logger
import Network.URI

data Flags = Flags
  { flagUri :: !URI,
    flagLogLevel :: !(Maybe LogLevel),
    flagFetchers :: !(Maybe Int),
    flagExternal :: !(Maybe Bool),
    flagCheckFragments :: !(Maybe Bool),
    flagMaxDepth :: !(Maybe Word)
  }
  deriving (Show, Eq)

data Settings = Settings
  { setUri :: !URI,
    setLogLevel :: !LogLevel,
    setFetchers :: !(Maybe Int),
    setExternal :: !Bool,
    setCheckFragments :: !Bool,
    setMaxDepth :: !(Maybe Word)
  }
  deriving (Show, Eq)
