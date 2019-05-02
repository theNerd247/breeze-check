{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.CheckInStatus where

-- NOTE: Do not change the order of this list
data CheckInStatus = CheckedOut 
                   | WaitingApproval CheckInGroupId 
                   | CheckedIn
                   | WaitingCreation CheckInGroupId TempPersonId
                   | SelectedForCheckIn
                   deriving (Show, Eq, Ord, Data, Generic)

makePrisms ''CheckInStatus

instance FromJSON CheckInStatus

instance ToJSON CheckInStatus

instance Default CheckInStatus where
  def = CheckedOut

