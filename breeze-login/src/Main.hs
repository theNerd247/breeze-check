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
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Data
import Data.Default
import Data.Maybe (listToMaybe)
import Data.Monoid
import GHC.Generics hiding (to)
import Network.HTTP.Simple
import Simple.Aeson (runAesonApi, fromBody)
import Simple.Snap
import Simple.String (fromParam, skipParse)
import Snap
import Snap.Snaplet.FastLogger
import qualified Data.ByteString.Char8 as Char8
import qualified Simple as Simple

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
  , _fastLogger :: Snaplet FastLogger
  }

makeLenses ''App

instance HasFastLogger App where
  logger = fastLogger

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

data Address = Address
  { _street :: String
  , _city :: String
  , _state :: String
  , _zipcode :: String
  } deriving (Show, Data, Generic) 

makeClassy ''Address

instance FromJSON Address

data NewPersonInfo = NewPersonInfo
  { _newPerson :: Person
  , _currentChurch :: String
  , _newAddress :: Address
  , _email :: String
  , _phone :: String
  } deriving (Show, Data, Generic)

makeClassy ''NewPersonInfo

instance FromJSON NewPersonInfo

instance HasAddress NewPersonInfo where
  address = newAddress

personFilter lname {-maddr-} = object $
  [ "189467778_last"   .= (lname :: String)
  {-, "697961327_street" .= (maddr^.street)-}
  {-, "697961327_city"   .= (maddr^.city)-}
  {-, "697961327_state"  .= (maddr^.state)-}
  {-, "697961327_zip"    .= (maddr^.zipcode)-}
  ]

class ToField a where
  toBreezeField :: a -> Object

data Field = forall a r. (ToJSON a, ToJSON r) => Field
  { field_id       :: String
  , field_type     :: String
  , field_response :: r
  , details        :: a
  }

instance ToJSON Field where
  toJSON (Field i t r d) = object $ 
    [ "field_id"       .= i , "field_type"     .= t
    , "field_response" .= r
    , "details"        .= d
    ]

addrField a = Field "697961327" "address" True $ object
  [ "street" .= (a^.street)
  , "city"   .= (a^.city)
  , "state"  .= (a^.state)
  , "zip"    .= (a^.zipcode)
  ]

currentChurchField c = Field "2105844304" "notes" c ()

runApiReq path params = do
  config <- asks (view breeze)
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
    [ ("person_id"   , Just . Char8.pack $ p^.pid)
    , ("instance_id" , Just . Char8.pack $ eventId)
    , ("direction"   , Just . Char8.pack $ p^.to checkInDir)
    ]
  where
    checkInDir p = if (p^.checkedIn) then "in" else "out"

makeNewPerson p = runApiReq "/people/add"
    [ ("first"       , Just . Char8.pack $ p^.newPerson.firstName)
    , ("last"        , Just . Char8.pack $ p^.newPerson.lastName)
    , ("fields_json" , Just . toStrict . encode $ mkFields)
    ]
  where
    mkFields = [addrField (p^.newAddress), currentChurchField (p^.currentChurch)]

defaultBreezeConfig :: Breeze
defaultBreezeConfig = def
    & apikey  .~ "e6e14e8a7e79bb7c62173b9879bacaee"
    & urlBase .~ "https://mountainviewmarietta.breezechms.com/api"
    & eventid .~ "36862980"

appInit :: SnapletInit App App
appInit = makeSnaplet "breeze-login" "a breeze login web app" Nothing $ do
  lgr <- nestSnaplet "" fastLogger $ initFastLoggerSnaplet (LogFileNoRotate "/tmp/breeze.log" 1000)
  addRoutes [("findperson", getPersonsHandle)] 
  return $ App 
    { _breezeApp = defaultBreezeConfig
    , _fastLogger = lgr
    }

checkInHandle :: (HasBreeze v) => Handler b v ()
checkInHandle = runAesonApi h
  where 
    h :: (HasBreeze v) => Handler b v Value 
    h =  do
      person <- fromBody
      checkin (person :: Person)

getPersonsHandle :: Handler App App ()
getPersonsHandle = runAesonApi $ do 
  lname <- skipParse <$> fromParam "lastname"
  writeLogger $ "/findperson?lastname=" ++ lname
  persons <- getPersonsWithFilter (personFilter lname)
  writeLogger $ "result: " ++ (show persons)
  return (persons :: Value)

withDefault :: (Simple.HasApi m) => String -> m String
withDefault = fmap skipParse . fromParam

addPersonHandle :: (HasBreeze v) => Handler b v ()
addPersonHandle = runAesonApi h 
  where 
    h :: (HasBreeze v) => Handler b v Value 
    h = do
      newPersonInfo <- fromBody
      makeNewPerson (newPersonInfo :: NewPersonInfo)

main = serveSnaplet defaultConfig appInit
