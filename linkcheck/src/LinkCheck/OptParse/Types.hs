module LinkCheck.OptParse.Types where

import Control.Monad.Logger
import Data.Aeson hiding ((<?>))
import Network.URI
import YamlParse.Applicative

data Flags = Flags
  { flagUri :: !URI,
    flagLogLevel :: !(Maybe LogLevel),
    flagFetchers :: !(Maybe Int),
    flagExternal :: !(Maybe Bool),
    flagCheckFragments :: !(Maybe Bool),
    flagMaxDepth :: !(Maybe Word)
  }
  deriving (Show, Eq)

data Configuration
  = Configuration
  deriving (Show, Eq)

data Environment
  = Environment
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = pure Configuration

data Settings = Settings
  { setUri :: !URI,
    setLogLevel :: !LogLevel,
    setFetchers :: !(Maybe Int),
    setExternal :: !Bool,
    setCheckFragments :: !Bool,
    setMaxDepth :: !(Maybe Word)
  }
  deriving (Show, Eq)
