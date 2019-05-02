{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Person where

import qualified Data.Text as Text
import Data.Address
import Data.CheckInStatus
import Data.Name
import GHC.Generics hiding (to)
import Control.Lens

type PersonId = Int

data Person = Person
  { _pid       :: PersonId
  , _personName :: Name
  , _checkedIn :: CheckInStatus
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
