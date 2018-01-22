{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Breeze where

import Control.Monad
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

data Breeze = Breeze
  { _apikey  :: String
  , _eventid :: String
  , _apiUrl :: String
  } deriving (Show, Data, Generic)

makeClassy ''Breeze

instance Default Breeze

data Person = Person
  { _pid       :: Id
  , _firstName :: String
  , _lastName  :: String
  , _checkedIn :: Bool
  , _pAddress  :: Maybe Address
  } deriving (Show, Data, Generic)

makeClassy ''Person

instance FromJSON Person where

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

instance Show CheckinDirection
  show In = "in"
  show Out = "out"

type Email = String
type EventId = String
type URL = String

data BreezeRequest =
  | FindPeople LastName (Maybe Address)
  | GetAttendance EventId
  | Checkin EventId Id CheckinDirection
  | NewPerson Person Address Email (Maybe ChurchInfo) (Maybe Phone)

newtype BreezeApiResponse a = FromBreeze { _fromBreeze :: a }

instance FromJSON (BreezeApiResponse Person) where
  parseJSON v@(Object o) = FromBreeze . Person
    <$> (o .: "id")
    <*> (o .: "first_name")
    <*> (o .: "last_name")
    <*> (pure False)
    <*> (pure Nothing)
  parseJSON _ = mempty

breezeIso :: Iso (BreezeApiResponse a) a
breezeIso = iso _fromBreeze FromBreeze

instance HasPerson (BreezeApiResponse Person) where
  person = breezeIso 

data AttendanceRecord = AttendanceRecord
  { _checkOutTime :: String
  , _aPid :: Id
  } deriving (Show, Data, Generic)

makeLenses ''AttendanceRecord

instance FromJSON AttendanceRecord where
  parseJSON (Object o) = AttendanceRecord
    <$> (o .: "created_on")
    <*> (o .: "check_out")
    <*> (o .: "person_id")

breezeRequestToParams :: BreezeRequest -> [(ByteString, Maybe ByteString)]
breezeRequestToParams = undefined

isCheckedIn :: Getter AttendanceRecord Bool
isCheckedIn = to $ \a -> a^.checkOutTime != "0000-00-00 00:00:00"
