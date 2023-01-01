module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data ToDo = ToDo
  { isComplete :: Bool,
    text :: !Text
  }
  deriving (Generic, Read, Show)

instance FromJSON ToDo

instance ToJSON ToDo