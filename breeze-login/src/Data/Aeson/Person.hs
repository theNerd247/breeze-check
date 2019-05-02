module Data.Aeson.Person where

import Data.Aeson
import Data.Aeson.Options ( customAesonOptions ) 
import Data.Person

instance FromJSON Person where
  parseJSON = genericParseJSON customAesonOptions

instance ToJSON Person where
  toJSON = genericToJSON customAesonOptions
