module LinkCheck.OptParse.Types where

import Data.Aeson hiding ((<?>))
import Network.URI
import YamlParse.Applicative

data Flags
  = Flags {flagUri :: URI}
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
  = Settings {setUri :: URI}
  deriving (Show, Eq)
