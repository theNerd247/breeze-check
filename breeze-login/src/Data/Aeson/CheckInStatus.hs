module Data.Aeson.CheckInStatus where

import Data.Aeson
import Data.CheckInStatus

instance FromJSON CheckInStatus

instance ToJSON CheckInStatus
