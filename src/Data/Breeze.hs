{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Breeze where

import Control.Lens hiding (Indexable)
import Control.Monad.Catch
import Data.Aeson 
import Data.Aeson.Types
import Data.IxSet
import Data.Data
import Elm hiding (Options, fieldLabelModifier, defaultOptions)
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

type CheckInGroupId = String
type ChurchInfo = String
type Email = String
type EventId = String
type FirstName = String
type LastName = String
type PersonId = String
type Phone = String

customAesonOptions = defaultOptions {fieldLabelModifier = removeUnderscorePrefix }

removeUnderscorePrefix ('_':xs) = xs
removeUnderscorePrefix xs = xs

data BreezeException = BreezeException { breezeErr :: String }
  deriving (Show, Generic, ElmType)

instance Exception BreezeException

instance ToJSON BreezeException

data AttendanceRecord = AttendanceRecord
  { _checkOutTime :: String
  , _aPid :: PersonId
  } deriving (Show, Eq, Ord, Data, Generic)

makeLenses ''AttendanceRecord

isCheckedIn :: Getter AttendanceRecord Bool
isCheckedIn = to $ \a -> a^.checkOutTime /= "0000-00-00 00:00:00"

instance FromJSON AttendanceRecord where
  parseJSON (Object o) = AttendanceRecord
    <$> (o .: "check_out")
    <*> (o .: "person_id")

instance Indexable AttendanceRecord where
  empty = ixSet 
    [ ixFun $ (:[]) . (view isCheckedIn)
    , ixFun $ (:[]) . (view aPid)
    ]

data Breeze = Breeze
  { _apiKey  :: String
  , _eventId :: Maybe EventId
  , _eventName :: Maybe String
  , _apiUrl :: String
  , _attendanceDB :: IxSet AttendanceRecord
  } deriving (Show, Data, Generic)

makeClassy ''Breeze

data Person = Person
  { _pid       :: PersonId
  , _firstName :: FirstName
  , _lastName  :: LastName
  , _checkedIn :: Bool
  } deriving (Show, Data, Generic, ElmType)

makeClassy ''Person

instance FromJSON Person where
  parseJSON v@(Object o) = Person
    <$> o .: "id" 
    <*> o .: "first_name" 
    <*> o .: "last_name" 
    <*> pure False
  parseJSON _ = mempty

instance ToJSON Person where
  toJSON = genericToJSON customAesonOptions

data Address = Address
  { _street :: String
  , _city :: String
  , _state :: String
  , _zipcode :: String
  } deriving (Show, Data, Generic, ElmType) 

makeClassy ''Address

instance FromJSON Address where
  parseJSON = genericParseJSON customAesonOptions

instance ToJSON Address where
  toJSON = genericToJSON customAesonOptions

data CheckinDirection = In | Out

instance Show CheckinDirection where
  show In = "in"
  show Out = "out"
data CheckInGroup = CheckInGroup
  { _checkInPersonIds :: [PersonId]
  , _checkInGroupId :: CheckInGroupId
  } deriving (Show, Data, Generic, ElmType)

