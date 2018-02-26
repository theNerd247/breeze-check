{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Breeze where

import Control.Concurrent.STM (TVar)
import Control.Lens hiding (Indexable, (.=))
import Control.Monad.Catch
import Data.Aeson 
import Data.Aeson.Types
import Data.Data
import Data.Default
import Data.IxSet
import Elm hiding (Options, fieldLabelModifier, defaultOptions)
import FastLogger (Logger)
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

-- NOTE: Do not change the order of this list
data CheckInStatus = CheckedOut 
                   | WaitingApproval CheckInGroupId 
                   | CheckedIn
                   deriving (Show, Eq, Ord, Data, Generic, ElmType)

makePrisms ''CheckInStatus

instance FromJSON CheckInStatus

instance ToJSON CheckInStatus

instance Default CheckInStatus where
  def = CheckedOut

data Name = Name
  { _firstName :: FirstName
  , _lastName  :: LastName
  } deriving (Show, Data, Eq, Ord, Generic, ElmType)

makeClassy ''Name

instance Default Name

instance FromJSON Name where
  parseJSON (Object o) = Name
    <$> (o .: "first_name")
    <*> (o .: "last_name")
  parseJSON _ = mempty

instance ToJSON Name where
  toJSON = genericToJSON customAesonOptions

data Address = Address
  { _street :: String
  , _city :: String
  , _state :: String
  , _zipcode :: String
  } deriving (Show, Eq, Ord, Data, Generic, ElmType) 

makeClassy ''Address

instance FromJSON Address where
  parseJSON = genericParseJSON customAesonOptions

instance ToJSON Address where
  toJSON = genericToJSON customAesonOptions

data NewPersonInfo = NewPersonInfo
  { _newAddress :: Address
  , _newCurrentChurch :: String
  , _newEmail :: String
  } deriving (Show, Eq , Ord, Data, Generic, ElmType)

makeLenses ''NewPersonInfo

instance FromJSON NewPersonInfo where
  parseJSON = genericParseJSON customAesonOptions

data Person = Person
  { _pid       :: PersonId
  , _personName :: Name
  , _checkedIn :: CheckInStatus
  , _newPersonInfo :: Maybe NewPersonInfo
  } deriving (Show, Data, Eq, Ord, Generic, ElmType)

makeClassy ''Person

instance HasName Person where
  name = personName


instance FromJSON Person where
  parseJSON v@(Object o) = Person
    <$> o .: "id" 
    <*> o .: "name"
    <*> (o .: "checkedIn" >>= return . toCheckin)
    <*> o .: "newPersonInfo"
    where
      toCheckin True = CheckedIn
      toCheckin False = CheckedOut
  parseJSON _ = mempty

instance ToJSON Person where
  toJSON p = object 
    [ "pid" .= (p^.pid)
    , "name" .= (p^.pid)
    , "checkedIn" .= (p^.checkedIn.to checkInStatusBool)
    , "newPersonInfo" .= (Nothing :: Maybe String)
    ]
    where
      checkInStatusBool CheckedIn = True
      checkInStatusBool (WaitingApproval _) = True
      checkInStatusBool CheckedOut = False

instance Default Person

newtype FName = FName String deriving (Eq, Ord, Data)

newtype LName = LName String deriving (Eq, Ord, Data)

instance Indexable Person where
  empty = ixSet 
    [ ixFun $ (:[]) . (view checkedIn)
    , ixFun $ (:[]) . (view pid)
    , ixFun $ \x -> x^? checkedIn . _WaitingApproval . to (:[]) ^. non []
    , ixFun $ (:[]) . FName . (view firstName)
    , ixFun $ (:[]) . LName . (view lastName)
    ]

newtype BreezePerson = BreezePerson 
    { _bPerson :: Person 
    } 
    deriving (Show, Eq, Ord, Data, Generic)

makeLenses ''BreezePerson

instance FromJSON BreezePerson where
  parseJSON v@(Object o) = fmap BreezePerson $ Person
    <$> o .: "id" 
    <*> parseJSON v
    <*> pure CheckedOut
    <*> pure Nothing
  parseJSON _ = mempty


newtype ParseAttendance = ParseAttendance { _attendingPerson :: Person }
  deriving (Show)

makeLenses ''ParseAttendance

instance FromJSON ParseAttendance where
  parseJSON v@(Object o) = fmap ParseAttendance $ Person
    <$> (o .: "person_id")
    <*> (o .: "details" >>= parseJSON)
    <*> (o .: "check_out" >>= return . parseCheckedOut )
    <*> pure Nothing
    where
      parseCheckedOut :: String -> CheckInStatus
      parseCheckedOut s 
        | s == "0000-00-00 00:00:00" = CheckedIn
        | otherwise = CheckedOut
  parseJSON x = typeMismatch "ParseAttendance" x

data EventInfo = EventInfo
  { _eid :: EventId
  , _ename :: String
  } deriving (Show, Eq, Ord, Data, Generic)

makeLenses ''EventInfo

instance FromJSON EventInfo where
  parseJSON (Object o) = EventInfo
    <$> (o .: "id")
    <*> (o .: "name")

data Breeze = Breeze
  { _apiKey  :: String
  , _eventId :: EventId
  , _eventName :: String
  , _apiUrl :: String
  , _personDB :: TVar (IxSet Person)
  , _infoLogger :: Logger
  , _infoLoggerCleanup :: IO ()
  , _errLogger :: Logger
  , _errLoggerCleanup :: IO ()
  , _checkInGroupCounter :: TVar Int
  , _debug :: Bool
  } deriving (Data, Generic)

makeClassy ''Breeze
