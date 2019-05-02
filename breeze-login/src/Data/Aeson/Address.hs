module Data.Aeson.Address where

import Data.Aeson
import Data.Aeson.Options ( customAesonOptions )
import Data.Address

instance FromJSON Address where
  parseJSON = genericParseJSON customAesonOptions

instance ToJSON Address where
  toJSON = genericToJSON customAesonOptions

