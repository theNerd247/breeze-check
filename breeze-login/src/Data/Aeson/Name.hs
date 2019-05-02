module Data.Aeson.Name where

import Data.Aeson
import Data.Aeson.Options ( customAesonOptions ) 
import Data.Name

instance FromJSON Name where
  parseJSON = genericParseJSON customAesonOptions

instance ToJSON Name where
  toJSON = genericToJSON customAesonOptions
