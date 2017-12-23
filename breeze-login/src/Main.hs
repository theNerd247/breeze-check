{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Control.Exception hiding (Handler)
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import Data.Aeson
import Control.Monad.Reader
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Data
import Data.Default
import Data.Maybe (listToMaybe)
import Data.Monoid
import GHC.Generics hiding (to)
import Network.HTTP.Simple
import Simple.Aeson (runAesonApi, fromBody)
import Simple.String (fromParam, skipParse)
import Simple.Snap
import Snap
import qualified Data.ByteString.Char8 as Char8

type Id = String

data Breeze = Breeze
  { _apikey  :: String
  , _eventid :: String
  , _urlBase :: String
  } deriving (Show, Data, Generic)

makeClassy ''Breeze

instance Default Breeze

data App = App
  { _breezeApp :: Breeze
  } deriving (Show, Data, Generic)

makeLenses ''App

instance Default App

instance HasBreeze App where
  breeze = breezeApp

data Person = Person
  { _pid       :: Id
  , _firstName :: String
  , _lastName  :: String
  , _checkedIn :: Bool
  } deriving (Show, Data, Generic)

makeClassy ''Person

instance FromJSON Person where
  parseJSON (Object o) = Person
    <$> (o .: "id")
    <*> (o .: "first_name")
    <*> (o .: "last_name")
    <*> (o .:? "checkedIn" .!= False)
  parseJSON _ = mempty

instance ToJSON Person

personFilter x = object $
  [ "189467778_last"   .= (x^.key "lastName")
  , "697961327_street" .= (x^.key "street")
  , "697961327_city"   .= (x^.key "city")
  , "697961327_state"  .= (x^.key "state")
  , "697961327_zip"    .= (x^.key "zip")
  ]

data Field = forall a r. (ToJSON a, ToJSON r) => Field
  { field_id       :: String
  , field_type     :: String
  , field_response :: r
  , details        :: a
  }

instance ToJSON Field where
  toJSON (Field i t r d) = object $ 
    [ "field_id"       .= i
    , "field_type"     .= t
    , "field_response" .= r
    , "details"        .= d
    ]

addrField a c s z = Field "697961327" "address" True $ object
  [ "street" .= a
  , "city"   .= c
  , "state"  .= s
  , "zip"    .= z
  ]

currentChurchField c = Field "2105844304" "notes" c ()

runApiReq path params = do
  config <- asks (breeze)
  req <- fmap (setHeaders (config^.apikey) . setRequestQueryString params) $ parseRequest ((config^.urlBase) ++ path)
  getResponseBody <$> httpJSON req
  where
    setHeaders apiKey =
        (addRequestHeader "Content-Type" "application/json")
      . (addRequestHeader "Api-Key" (Char8.pack apiKey))

getPersonsWithFilter filter = runApiReq "/people" 
    [ ("filter_json", Just . toStrict . encode $ filter)
    , ("details", Just . Char8.pack . show $ 0)
    ]

checkin p = do
  eventId <- asks (view eventid)
  runApiReq "/events/attendance/add"
    [ ("person_id"   , Just . Char8.pack $ p^pid)
    , ("instance_id" , Just . Char8.pack $ eventId)
    , ("direction"   , Just . Char8.pack $ p^.to checkInDir)
    ]
  where
    checkInDir p = if (p^.checkedIn) then "in" else "out"

makeNewPerson f l a c s z cs = runApiReq "/people/add"
    [ ("first"       , Just . Char8.pack $ f)
    , ("last"        , Just . Char8.pack $ l)
    , ("fields_json" , Just . toStrict . encode $ mkFields)
    ]
  where
    mkFields = [addrField a c s z, currentChurchField cs]

appInit :: SnapletInit App App
appInit = makeSnaplet "breeze-login" "a breeze login web app" Nothing $ do
  addRoutes [("lastname", lastNameHandle)] 
  return $ def
    & apikey  .~ "e6e14e8a7e79bb7c62173b9879bacaee"
    & urlBase .~ "https://mountainviewmarietta.breezechms.com/api"
    & eventid .~ "36862980"

checkInHandle :: (HasBreeze v) => Handle b v ()
checkInHandle = do
  persons <- fromBody
  forM_ persons checkin

filterHandle :: (HasBreeze v) => Handle b v [Person]
filterHandle = do
  filter <- fromBody
  getPersonsWithFilter (personFilter filter)

main = serveSnaplet defaultConfig appInit
