{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Person where

import qualified Data.Text as Text
import Data.Address

type PersonId = Int

data Person = Person
  { _pid       :: PersonId
  , _personName :: Text.Text
  , _checkedIn :: CheckInStatus
  , _newPersonInfo :: Maybe NewPersonInfo
  , _wantsPhotos :: Bool
  , _isParent :: Bool
  , _newAddress :: Maybe Address
  , _newCurrentChurch :: Maybe Text.Text
  , _newEmail :: Maybe Text.Text
  , _fullyNew :: Bool
  } deriving (Show, Data, Eq, Ord, Generic)

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

