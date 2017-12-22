#!/usr/bin/env stack
{- stack 
  --resolver lts-9.14 
  --nix 
  --nix-packages zlib
  exec ghci
  --package http-conduit
  --package aeson
  --package bytestring
  --package aeson-lens
  --package lens
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Data
import Data.Maybe (listToMaybe)
import Data.Monoid
import GHC.Generics hiding (to)
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as Char8

apiKey = "e6e14e8a7e79bb7c62173b9879bacaee"
urlBase = "https://mountainviewmarietta.breezechms.com/api"
eventId = 36862980

data LastNameFilter = LastNameFilter
  { _lastName :: String
  } deriving (Show, Data, Generic)

makeLenses ''LastNameFilter

data Address = Address
  { _address :: String
  , _city    :: String
  , _state   :: String
  , _zipCode :: String
  }

makeLenses ''Address

data Field = forall a r. (ToJSON a, ToJSON r) => Field
  { field_id :: String
  , field_type :: String
  , field_response :: r
  , details :: a
  }

instance ToJSON Address where
  toJSON x = object $ 
    [ "697961327_street" .= (x^.address)
    , "697961327_city"   .= (x^.city)
    , "697961327_state"  .= (x^.state)
    , "697961327_zip"    .= (x^.zipCode)
    ]

instance ToJSON LastNameFilter where
  toJSON p = object $ 
    [ "189467778_last" .= (p^.lastName)
    ] -- "189467778_first" .= (_first p)

instance ToJSON Field where
  toJSON (Field i t r d) = object $ 
    [ "field_id" .= i
    , "field_type" .= t
    , "field_response" .= r
    , "details" .= d
    ]

addrField a c s z = Field "697961327" "address" True $ object
  [ "street" .= a
  , "city"   .= c
  , "state"  .= s
  , "zip"    .= z
  ]

currentChurchField c = Field "2105844304" "notes" c ()

setHeaders = 
    (addRequestHeader "Content-Type" "application/json")
  . (addRequestHeader "Api-Key" apiKey)

runApiReq path params = do
  req <- fmap (setHeaders . setRequestQueryString params) $ parseRequest (urlBase ++ path)
  getResponseBody <$> httpJSON req

getPersonsByAddress addr c s z = getPersonsWithFilter (Address addr c s z)

getPersonsByLastName lname = getPersonsWithFilter (LastNameFilter lname)

filterPersons ps as = as^..folded.filtered (getAny . mconcat . findFirst)
  where
    findFirst x = Any . (\a -> (a^.to Just . key "id" :: Maybe String) == (x^.to Just . key "id" :: Maybe String)) <$> ps

getPersonsWithFilter filter = runApiReq "/people" 
    [ ("filter_json", Just . toStrict . encode $ filter)
    , ("details", Just . Char8.pack . show $ 0)
    ]

checkin uuid = runApiReq "/events/attendance/add" 
  [ ("person_id", Just . Char8.pack $ uuid)
  , ("instance_id", Just . Char8.pack . show $ eventId)
  ]

makeNewPerson f l a c s z cs = runApiReq "/people/add"
    [ ("first"       , Just . Char8.pack $ f)
    , ("last"        , Just . Char8.pack $ l)
    , ("fields_json" , Just . toStrict . encode $ mkFields)
    ]
  where
    mkFields = [addrField a c s z, currentChurchField cs]

main = putStrLn . show =<< (getPersonsWithFilter (Address "" "Kennesaw" "" "") :: IO Value)
