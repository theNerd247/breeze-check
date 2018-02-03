{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Breeze where

import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Except
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.Breeze
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Default
import Data.Foldable (fold)
import Data.IxSet
import Data.Proxy
import Data.Text (Text)
import Data.Time
import Simple.Aeson (runAesonApi, fromBody)
import Simple.Snap
import Simple.String (fromParam, skipParse)
import Snap
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import qualified Network.HTTP.Simple as HTTP

data FindPeople = FindPeople LastName (Maybe Address)
data GetAttendance = GetAttendance EventId
data Checkin = Checkin EventId PersonId CheckinDirection
data NewPerson = NewPerson FirstName LastName Address Email (Maybe ChurchInfo) (Maybe Phone)
data GetEvents = GetEvents

class HasBreezeApp b where
  breezeLens :: SnapletLens b Breeze

instance FromJSON NewPerson where
  parseJSON (Object o) = NewPerson
    <$> o .: "firstName"
    <*> o .: "lastName"
    <*> o .: "address"
    <*> o .: "email"
    <*> o .: "churchInfo"
    <*> o .: "phone"
  parseJSON _ = mempty

class BreezeApi a where
  type BreezeResponse a :: *
  runBreeze :: (MonadIO m, MonadThrow m) => Breeze -> a -> m (Either HTTP.JSONException (BreezeResponse a))

instance BreezeApi FindPeople where
  type BreezeResponse FindPeople = [Person]
  runBreeze b (FindPeople lname maddr) = runApiReq b "/people" $
    [ ("details", Just . Char8.pack . show $ 0)
    , ("filter_json", Just . toStrict . encode $ object
        [ "189467778_last"   .= lname
        {-, "697961327_street" .= (maddr^.street)-}
        {-, "697961327_city"   .= (maddr^.city)-}
        {-, "697961327_state"  .= (maddr^.state)-}
        {-, "697961327_zip"    .= (maddr^.zipcode)-}
        ])
    ]

instance BreezeApi GetAttendance where
  type BreezeResponse GetAttendance = [AttendanceRecord]
  runBreeze b (GetAttendance eid) = runApiReq b "/events/attendance/list"
    [ ("instance_id", Just . Char8.pack $ eid)
    , ("details",     Just "false")
    , ("type",        Just "person")
    ]
  
instance BreezeApi Checkin where
  type BreezeResponse Checkin = Value
  runBreeze b (Checkin eid pid chkInDir) = runApiReq b "/events/attendance/add"
    [ ("person_id"   , Just . Char8.pack $ pid)
    , ("instance_id" , Just . Char8.pack $ eid)
    , ("direction"   , Just . Char8.pack . show $ chkInDir)
    ]

instance BreezeApi NewPerson where
  type BreezeResponse NewPerson = Person
  runBreeze b (NewPerson f l a email mcinfo mphone) = runApiReq b "/people/add"
    [ ("first"       , Just . Char8.pack $ f)
    , ("last"        , Just . Char8.pack $ l)
    , ("fields_json" , Just . toStrict . encode $
        [ field "697961327" "address" True $ object
            [ "street" .= (a^.street)
            , "city"   .= (a^.city)
            , "state"  .= (a^.state)
            , "zip"    .= (a^.zipcode)
            ]
            -- TODO: add missing fields
        ])
    ]
    where
      field :: String -> String -> Bool -> Value -> Value
      field i t r d = object
        [ "field_id"       .= i 
        , "field_type"     .= t
        , "field_response" .= r
        , "details"        .= d
        ]

runApiReq config path params = do
  let modReq = HTTP.addRequestHeader "Content-Type" "application/json"
            . HTTP.addRequestHeader "Api-Key" (Char8.pack $ config^.apiKey)
            . HTTP.setRequestQueryString params
  req <- HTTP.parseRequest ((config^.apiUrl) ++ (addRoot path))
  HTTP.getResponseBody <$> HTTP.httpJSONEither (modReq req)
  where
    addRoot xs@('/':_) = xs
    addRoot xs = '/':xs

runBreeze' a = do 
  config <- use breeze
  x <- runBreeze config a
  either throwM return $ x

getAttendance' :: (MonadIO m, MonadThrow m) => Breeze -> m (Either HTTP.JSONException Breeze)
getAttendance' config = maybe (return $ Right config) getAs $ config^.eventId
  where
    getAs eid = do
      eas <- runBreeze config $ GetAttendance eid
      return $ fmap (mergeAttendance . filter (\p -> (p^.aCheckedIn) == True)) eas
      where
        mergeAttendance :: [AttendanceRecord] -> Breeze
        mergeAttendance as = config & personDB %~ (fold $ updatePerson <$> as)

        updatePerson :: AttendanceRecord -> IxSet Person -> IxSet Person
        updatePerson p db = maybe 
          (db)
          (\x -> updateIx (x^.pid) (x & checkedIn .~ True) db)
          (getOne $ db @= (p^.aPid) @= False)

getPersonsHandle :: (HasBreezeApp b) => Handler b v ()
getPersonsHandle = withTop breezeLens $ runAesonApi $ do 
  lname <- skipParse <$> fromParam "lastname"
  persons <- runBreeze' $ FindPeople lname Nothing
  return $ filter (\p -> List.isPrefixOf lname $ p^.lastName ) persons

addPersonHandle :: (HasBreezeApp b)  => Handler b v ()
addPersonHandle = withTop breezeLens $ runAesonApi $ do
  newPersonInfo <- fromBody
  person <- runBreeze' (newPersonInfo :: NewPerson)
  return person

defaultBreeze :: Breeze
defaultBreeze = Breeze
  { _apiKey = "e6e14e8a7e79bb7c62173b9879bacaee"
  , _apiUrl = "https://mountainviewmarietta.breezechms.com/api"
  , _eventId = Nothing
  , _eventName = Nothing
  , _personDB = empty
  }

initEvent :: Breeze -> IO (Either Text Breeze)
initEvent config = runExceptT $ do
  es <- getEs
  eid <- maybe (throwE "Couldn't fetch event id") return $ es^. nth 0 . key "id"
  ename <- maybe (throwE "Couldn't fetch event name") return $ es^. nth 0 . key "name"
  withExceptT (const "Failed to get attendance") . ExceptT $ getAttendance' $ config 
              & eventId .~ (Just eid) 
              & eventName .~ (Just ename)
  where
    getEs :: ExceptT Text IO (Maybe Value)
    getEs = withExceptT (const "Failed to fetch event list") $ do
      now <- liftIO getCurrentTime
      let s = formatTime defaultTimeLocale "%F" (now {utctDay = addDays (-1) (utctDay now)})
      let e = formatTime defaultTimeLocale "%F" now
      ExceptT $ runApiReq config "/events" 
          [("start", Just . Char8.pack $ s)
          , ("end", Just . Char8.pack $ e)
          ]
 

initBreeze :: (HasBreezeApp b) => SnapletInit b Breeze
initBreeze = makeSnaplet "breeze checkin" "a breeze chms mobile friendly checkin system" Nothing $ do
  addRoutes 
    [ ("findperson", getPersonsHandle)
    ] 
  addPostInitHook initEvent
  return defaultBreeze
