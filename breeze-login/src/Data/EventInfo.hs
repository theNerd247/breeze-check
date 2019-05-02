{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.EventInfo where

import qualified Data.Text as Text

data EventInfo = EventInfo
  { _eventId :: Text.Text
  , _eventName :: Text.Text
  } deriving (Show, Eq, Ord, Data, Generic)

makeClassy ''EventInfo