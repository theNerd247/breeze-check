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
import qualified Data.Text as Text
import Data.Default
import Data.IxSet
import Elm hiding (Options, fieldLabelModifier, defaultOptions)
import Text.Read (readEither)
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
type ChurchInfo = Text.Text
type Email = Text.Text
type EventId = Text.Text
type FirstName = Text.Text
type LastName = Text.Text
type PersonId = Int
type Phone = Text.Text
type TempPersonId = PersonId
type IsParent = Bool

customAesonOptions = defaultOptions {fieldLabelModifier = removeUnderscorePrefix }

removeUnderscorePrefix ('_':xs) = xs
removeUnderscorePrefix xs = xs

instance Default (Text.Text) where
  def = mempty

data BreezeException = BreezeException { breezeErr :: String }
  deriving (Show, Generic, ElmType)

instance Exception BreezeException

instance ToJSON BreezeException

-- NOTE: Do not change the order of this list
data CheckInStatus = CheckedOut 
                   | WaitingApproval CheckInGroupId 
                   | CheckedIn
                   | WaitingCreation CheckInGroupId TempPersonId
                   | SelectedForCheckIn
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
  parseJSON = genericParseJSON customAesonOptions

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
  , _newCurrentChurch :: Maybe String
  , _newEmail :: String
  , _fullyNew :: Bool
  } deriving (Show, Eq , Ord, Data, Generic, ElmType)

makeLenses ''NewPersonInfo

instance FromJSON NewPersonInfo where
  parseJSON = genericParseJSON customAesonOptions

instance ToJSON NewPersonInfo where
  toJSON = genericToJSON customAesonOptions

instance HasAddress NewPersonInfo where
  address = newAddress

data Person = Person
  { _pid       :: PersonId
  , _personName :: Name
  , _checkedIn :: CheckInStatus
  , _newPersonInfo :: Maybe NewPersonInfo
  , _wantsPhotos :: Bool
  , _isParent :: Bool
  } deriving (Show, Data, Eq, Ord, Generic, ElmType)

makeClassy ''Person

instance HasName Person where
  name = personName

instance FromJSON Person where
  parseJSON = genericParseJSON customAesonOptions
  {-parseJSON v@(Object o) = Person-}
    {-<$> o .: "pid"-}
    {-<*> o .: "name"-}
    {-<*> (o .: "checkedIn" >>= return . toCheckin)-}
    {-<*> o .: "newPersonInfo"-}
    {-<*> o .: "wantsPhotos"-}
    {-where-}
      {-toCheckin True = CheckedIn-}
      {-toCheckin False = CheckedOut-}
  {-parseJSON _ = mempty-}

instance ToJSON Person where
  toJSON = genericToJSON customAesonOptions
  {-toJSON p = object -}
    {-[ "pid" .= (p^.pid)-}
    {-, "name" .= (p^.personName)-}
    {-, "checkedIn" .= (p^.checkedIn.to checkInStatusBool)-}
    {-, "newPersonInfo" .= (Nothing :: Maybe String)-}
    {-, "wantsPhotos" .= (p^.wantsPhotos)-}
    {-]-}
    {-where-}
      {-checkInStatusBool CheckedOut = False-}
      {-checkInStatusBool _ = True-}

parseIdFromText :: String -> Object -> Parser Int
parseIdFromText k o =
  (o .: (Text.pack k))
    >>= withText k tparser
  where
    tparser :: Text.Text -> Parser Int
    tparser t = 
      either 
        (flip typeMismatch (String t)) 
        return 
        (readEither . Text.unpack $ t)


instance Default Bool where
  def = False

instance Default Person

newtype FName = FName Text.Text deriving (Eq, Ord, Data)

newtype LName = LName Text.Text deriving (Eq, Ord, Data)

newtype GID = GID CheckInGroupId deriving (Eq, Ord, Data)


instance Indexable Person where
  empty = ixSet 
    [ ixFun $ (:[]) . (view checkedIn)
    , ixFun $ (:[]) . (view pid)
    , ixFun $ \x -> GID <$> case x^.checkedIn of
                      WaitingApproval gid -> [gid]
                      WaitingCreation gid _ -> [gid]
                      _ -> []
    , ixFun $ (:[]) . FName . (view firstName)
    , ixFun $ (:[]) . LName . (view lastName)
    , ixFun $ (:[]) . (view  newPersonInfo)
    ]

newtype BreezePerson = BreezePerson 
    { _bPerson :: Person
    } 
    deriving (Show, Eq, Ord, Data, Generic)

makeLenses ''BreezePerson

instance FromJSON BreezePerson where
  parseJSON = withObject "BreezePerson" $ \o -> fmap BreezePerson $ Person
    <$> (parseIdFromText "id" o)
    <*> parseName o
    <*> pure CheckedOut
    <*> pure Nothing
    <*> pure False
    <*> pure False

parseName :: Object -> Parser Name
parseName o = Name
    <$> (o .: "first_name")
    <*> (o .: "last_name")


newtype ParseAttendance = ParseAttendance { _attendingPerson :: Person }
  deriving (Show)

makeLenses ''ParseAttendance

instance FromJSON ParseAttendance where
  parseJSON = withObject "ParseAttendance" $ \o -> fmap ParseAttendance $ Person
    <$> (parseIdFromText "person_id" o)
    <*> (o .: "details" >>= parseName)
    <*> (o .: "check_out" >>= return . parseCheckedOut )
    <*> pure Nothing
    <*> pure False
    <*> pure False
    where
      parseCheckedOut :: String -> CheckInStatus
      parseCheckedOut s 
        | s == "0000-00-00 00:00:00" = CheckedIn
        | otherwise = CheckedOut

data EventInfo = EventInfo
  { _eventId :: EventId
  , _eventName :: String
  } deriving (Show, Eq, Ord, Data, Generic, ElmType)

makeClassy ''EventInfo

instance FromJSON EventInfo where
  parseJSON (Object o) = EventInfo
    <$> (o .: "id")
    <*> (o .: "name")

instance ToJSON EventInfo where
  toJSON = genericToJSON customAesonOptions

instance Default EventInfo

data Breeze = Breeze
  { _apiKey  :: String
  , _apiUrl :: String
  , _personDB :: TVar (IxSet Person)
  , _infoLogger :: Logger
  , _infoLoggerCleanup :: IO ()
  , _errLogger :: Logger
  , _errLoggerCleanup :: IO ()
  , _checkInGroupCounter :: TVar Int
  , _debug :: TVar Bool
  , _breezeEventInfo :: TVar EventInfo
  } deriving (Data, Generic)

makeClassy ''Breeze
