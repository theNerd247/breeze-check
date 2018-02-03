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
import Data.Data
import Data.Default
import Data.IxSet
import Elm hiding (Options, fieldLabelModifier, defaultOptions)
import GHC.Generics hiding (to)

{- Breeze Api Interface
   ===============

From        : Front End                                                      -> To Breeze Api                                                  -> From Breeze Api             -> To Front end
Find People : {lastname, address?}                                           -> {lastname, address?}                                           -> {[id, lastname, firstname]} -> {[id, lastname, firstname, checkedin]}
Attendance  :                                                                -> {eventInstanceId}                                              -> {[checkedIn, id]}                    -> {}
Checkin     : {id, direction}                                                -> {id, eventInstanceId, direction}                               -> {bool}                      -> {id, firstname, lastname, checkedin}
New Family  : {lastname, [firstname], currentChurch, email, phone?, address} -> {[lastname, firstname, currentChurch, email, phone?, address]} -> {id, lastname, firstname}   -> {[id, firstname, lastname, checkedin]}

-}

type CheckInGroupId = Int
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
  { _aCheckedIn :: Bool
  , _aPid :: PersonId
  } deriving (Show, Eq, Ord, Data, Generic)

makeLenses ''AttendanceRecord

instance FromJSON AttendanceRecord where
  parseJSON (Object o) = AttendanceRecord
    <$> (o .: "check_out" >>= return . (/= ("0000-00-00 00:00:00" :: String)))
    <*> (o .: "person_id")

data Person = Person
  { _pid       :: PersonId
  , _firstName :: FirstName
  , _lastName  :: LastName
  , _checkedIn :: Bool
  , _groupId   :: Maybe CheckInGroupId
  } deriving (Show, Data, Eq, Ord, Generic, ElmType)

makeClassy ''Person

newtype FName = FName String deriving (Eq, Ord, Data)

newtype LName = LName String deriving (Eq, Ord, Data)

instance Indexable Person where
  empty = ixSet 
    [ ixFun $ (:[]) . (view checkedIn)
    , ixFun $ (:[]) . (view pid)
    , ixFun $ (:[]) . (view groupId)
    , ixFun $ (:[]) . FName . (view firstName)
    , ixFun $ (:[]) . LName . (view lastName)
    ]

instance FromJSON Person where
  parseJSON v@(Object o) = Person
    <$> o .: "id" 
    <*> o .: "first_name" 
    <*> o .: "last_name" 
    <*> pure False
    <*> pure Nothing
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

data Breeze = Breeze
  { _apiKey  :: String
  , _eventId :: Maybe EventId
  , _eventName :: Maybe String
  , _apiUrl :: String
  , _personDB :: IxSet Person
  } deriving (Show, Data, Generic)

makeClassy ''Breeze