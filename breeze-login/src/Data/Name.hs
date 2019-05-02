{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Name where

import qualified Data.Text as Text

data Name = Name
  { _firstName :: Text.Text
  , _lastName  :: Text.Text
  } deriving (Show, Data, Eq, Ord, Generic)

makeClassy ''Name

instance FromJSON Name where
  parseJSON = genericParseJSON customAesonOptions

instance ToJSON Name where
  toJSON = genericToJSON customAesonOptions
