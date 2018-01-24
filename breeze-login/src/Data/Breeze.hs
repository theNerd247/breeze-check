{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Breeze where

import Control.Lens
import Data.Aeson
import Data.Data
import Data.Default
import GHC.Generics hiding (to)

{- Breeze Api Interface
   ===============

From        : Front End                                                      -> To Breeze Api                                                  -> From Breeze Api             -> To Front end
Find People : {lastname, address?}                                           -> {lastname, address?}                                           -> {[id, lastname, firstname]} -> {[id, lastname, firstname, checkedin]}
Attendance  :                                                                -> {eventInstanceId}                                              -> {[checkedIn, id]}                    -> {}
Checkin     : {id, direction}                                                -> {id, eventInstanceId, direction}                               -> {bool}                      -> {id, firstname, lastname, checkedin}
New Family  : {lastname, [firstname], currentChurch, email, phone?, address} -> {[lastname, firstname, currentChurch, email, phone?, address]} -> {id, lastname, firstname}   -> {[id, firstname, lastname, checkedin]}

-}

type Id = String
type LastName = String
type FirstName = String
type ChurchInfo = String
type Phone = String
type Email = String
type EventId = String

data Breeze = Breeze
  { _apiKey  :: String
  , _eventId :: String
  , _apiUrl :: String
  } deriving (Show, Data, Generic)

makeClassy ''Breeze

instance Default Breeze

data Person = Person
  { _pid       :: Id
  , _firstName :: FirstName
  , _lastName  :: LastName
  , _checkedIn :: Bool
  } deriving (Show, Data, Generic)

makeClassy ''Person

instance FromJSON Person where
  parseJSON v@(Object o) = Person
    <$> o .: "id" 
    <*> o .: "first_name" 
    <*> o .: "last_name" 
    <*> pure False
  parseJSON _ = mempty

instance ToJSON Person

data Address = Address
  { _street :: String
  , _city :: String
  , _state :: String
  , _zipcode :: String
  } deriving (Show, Data, Generic) 

makeClassy ''Address

instance FromJSON Address
instance ToJSON Address

data CheckinDirection = In | Out

instance Show CheckinDirection where
  show In = "in"
  show Out = "out"

data AttendanceRecord = AttendanceRecord
  { _checkOutTime :: String
  , _aPid :: Id
  } deriving (Show, Data, Generic)

makeLenses ''AttendanceRecord

instance FromJSON AttendanceRecord where
  parseJSON (Object o) = AttendanceRecord
    <$> (o .: "check_out")
    <*> (o .: "person_id")

isCheckedIn :: Getter AttendanceRecord Bool
isCheckedIn = to $ \a -> a^.checkOutTime /= "0000-00-00 00:00:00"
