module Data.Aeson.EventInfo where

import Data.Aeson
import Data.Aeson.Options ( customAesonOptions ) 
import Data.EventInfo

instance FromJSON EventInfo where
  parseJSON (Object o) = EventInfo
    <$> (o .: "id")
    <*> (o .: "name")

instance ToJSON EventInfo where
  toJSON = genericToJSON customAesonOptions
