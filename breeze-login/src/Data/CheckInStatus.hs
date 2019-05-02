{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.CheckInStatus where

import GHC.Generics hiding (to)
import Control.Lens

type CheckInGroupId = Int

-- NOTE: Do not change the order of this list
data CheckInStatus = CheckedOut 
                   | WaitingApproval CheckInGroupId 
                   | CheckedIn
                   | WaitingCreation CheckInGroupId Int
                   | SelectedForCheckIn
                   deriving (Show, Eq, Ord, Data, Generic)

makePrisms ''CheckInStatus
