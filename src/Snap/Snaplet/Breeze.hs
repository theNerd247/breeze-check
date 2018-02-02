{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Breeze where

import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.Breeze
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.IxSet
import Data.Proxy
import Data.Time
import Simple.Snap
import Snap
import Snap.Snaplet.FastLogger
import qualified Data.ByteString.Char8 as Char8
import qualified Network.HTTP.Simple as HTTP

data FindPeople = FindPeople LastName (Maybe Address)
data GetAttendance = GetAttendance EventId
data Checkin = Checkin EventId PersonId CheckinDirection
data NewPerson = NewPerson FirstName LastName Address Email (Maybe ChurchInfo) (Maybe Phone)

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
  runBreeze :: (HasBreeze v) => a -> Handler b v (BreezeResponse a)

instance BreezeApi FindPeople where
  type BreezeResponse FindPeople = [Person]
  runBreeze (FindPeople lname maddr) = runApiReq "/people" $
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
  runBreeze (GetAttendance eid) = runApiReq "/events/attendance/list"
    [ ("instance_id", Just . Char8.pack $ eid)
    , ("details",     Just "false")
    , ("type",        Just "person")
    ]
  
instance BreezeApi Checkin where
  type BreezeResponse Checkin = Value
  runBreeze (Checkin eid pid chkInDir) = runApiReq "/events/attendance/add"
    [ ("person_id"   , Just . Char8.pack $ pid)
    , ("instance_id" , Just . Char8.pack $ eid)
    , ("direction"   , Just . Char8.pack . show $ chkInDir)
    ]

instance BreezeApi NewPerson where
  type BreezeResponse NewPerson = Person
  runBreeze (NewPerson f l a email mcinfo mphone) =  runApiReq "/people/add"
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

runApiReq' config path params = do
  let req = HTTP.defaultRequest & 
          HTTP.setRequestPath (Char8.pack $ (config^.apiUrl) ++ path)
        . HTTP.setRequestMethod "GET"
        . HTTP.addRequestHeader "Content-Type" "application/json"
        . HTTP.addRequestHeader "Api-Key" (Char8.pack $ config^.apiKey)
        . HTTP.setRequestQueryString params
  HTTP.getResponseBody <$> HTTP.httpJSONEither req

runApiReq path params = do 
  config <- use breeze
  r <- runApiReq' config path params
  either throwM return $ r

getAttendance :: (HasBreeze v, HasFastLogger b) => Handler b v ()
getAttendance = do
  eid <- getEid
  as <- runBreeze $ GetAttendance eid
  assign (breeze.attendanceDB) (fromList as)

{-signIn :: (HasBreeze v, HasFastLogger b) => [PersonId] -> Handler b v ()-}
{-signIn = do-}

getEid :: (HasBreeze v) => Handler b v EventId
getEid = use (breeze.eventId) >>= maybe 
  (throwM $ BreezeException "there is no configured event")
  return

defaultBreeze :: Breeze
defaultBreeze = Breeze
  { _apiKey = "e6e14e8a7e79bb7c62173b9879bacaee"
  , _apiUrl = "https://mountainviewmarietta.breezechms.com/api"
  , _eventId = Nothing
  , _eventName = Nothing
  , _attendanceDB = empty
  }

breezeInit :: SnapletInit b Breeze
breezeInit = makeSnaplet "breeze checkin" "a breeze chms mobile friendly checkin system" Nothing $ do
  now <- liftIO getCurrentTime
  let s = formatTime defaultTimeLocale "%F" now
  let e = formatTime defaultTimeLocale "%F" now
  es <- runApiReq' defaultBreeze "events" [("start", Just . Char8.pack $ s)
                           , ("end", Just . Char8.pack $ e)
                           ]
  return $ defaultBreeze 
    & eventId .~ (es^. _Right . nth 0 . key "id")
    & eventName .~ (es^. _Right . nth 0 . key "name")
