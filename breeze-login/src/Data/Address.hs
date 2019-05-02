{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Address where

import qualified Data.Text as Text

data Address = Address
  { _street :: Text.Text
  , _city :: Text.Text
  , _state :: Text.Text
  , _zipcode :: Int
  } deriving (Show, Eq, Ord, Data, Generic, ElmType) 

makeClassy ''Address

instance FromJSON Address where
  parseJSON = genericParseJSON customAesonOptions

instance ToJSON Address where
  toJSON = genericToJSON customAesonOptions
