module LinkCheck.OptParse.Types where

import Data.Aeson hiding ((<?>))
import YamlParse.Applicative

data Flags
  = Flags
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

data Settings
  = Settings
  deriving (Show, Eq)
