{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.EventInfo where

import qualified Data.Text as Text
import GHC.Generics hiding (to)
import Control.Lens

data EventInfo = EventInfo
  { _eventId :: Text.Text
  , _eventName :: Text.Text
  } deriving (Show, Eq, Ord, Data, Generic)

makeClassy ''EventInfo
