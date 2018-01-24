{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Breeze where

import Control.Monad.IO.Class
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Breeze
import Data.Proxy
import Data.ByteString.Lazy.Char8 (toStrict)
import qualified Data.ByteString.Char8 as Char8
import qualified Network.HTTP.Simple as HTTP
import Control.Monad.Reader.Class
import Control.Monad.Catch

data FindPeople = FindPeople LastName (Maybe Address)
data GetAttendance = GetAttendance EventId
data Checkin = Checkin EventId Id CheckinDirection
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
  runBreeze :: (MonadThrow m, HasBreeze r, MonadReader r m, MonadIO m) => a -> m (String, (BreezeResponse a))

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

runApiReq path params = do
  config <- asks (view breeze)
  let buildRequest = 
          HTTP.addRequestHeader "Content-Type" "application/json"
        . HTTP.addRequestHeader "Api-Key" (Char8.pack $ config^.apiKey)
        . HTTP.setRequestQueryString params
  req <- buildRequest <$> HTTP.parseRequest ((config^.apiUrl) ++ path)
  resp <- HTTP.getResponseBody <$> HTTP.httpJSON req
  return (show req, resp)
