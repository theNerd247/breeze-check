{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Snap.Snaplet.Breeze where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Except
import Control.Monad (forM)
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
data Checkin = Checkin EventId PersonId 
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
  runBreeze :: (MonadIO m, MonadThrow m) => Breeze -> a -> m (BreezeResponse a)

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
  type BreezeResponse GetAttendance = [ParseAttendance]
  runBreeze b (GetAttendance eid) = runApiReq b "/events/attendance/list"
    [ ("instance_id", Just . Char8.pack $ eid)
    , ("details",     Just "false")
    , ("type",        Just "person")
    ]
  
instance BreezeApi Checkin where
  type BreezeResponse Checkin = Bool
  runBreeze b (Checkin eid pid) = runApiReq b "/events/attendance/add"
    [ ("person_id"   , Just . Char8.pack $ pid)
    , ("instance_id" , Just . Char8.pack $ eid)
    , ("direction"   , Just "in")
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
  either throwM return =<< HTTP.getResponseBody <$> HTTP.httpJSONEither (modReq req)
  where
    addRoot xs@('/':_) = xs
    addRoot xs = '/':xs

runBreeze' a = do 
  config <- use breeze
  runBreeze config a

getAttendance' :: (MonadIO m, MonadThrow m) => Breeze -> m ()
getAttendance' config = flip evalStateT config $ do
  let eid = config^.eventId
  eas <- runBreeze' $ GetAttendance eid
  withTVarWrite personDB $ fold $
    eas^..folded.to attendingPerson & mapped %~ updateAttending
  where
    updateAttending :: Person -> IxSet Person -> IxSet Person
    updateAttending p db = maybe
      (insert p db)
      (\x -> updateIx (x^.pid) (x & checkedIn .~ max (x^.checkedIn) (p^.checkedIn)) db)
      (getOne $ db @= (p^.pid))
    
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

updatePerson :: (Person -> Person) -> Person -> IxSet Person -> IxSet Person
updatePerson f p = updateIx (p^.pid) (f p)

withTVarRead :: (MonadState s m, MonadIO m) => Lens' s (TVar a) -> (a -> b) -> m b
withTVarRead l f = use l >>= liftIO . atomically . fmap f . readTVar 

withTVarWrite :: (MonadState s m, MonadIO m) => Lens' s (TVar a) -> (a -> a) -> m ()
withTVarWrite l f = use l >>= liftIO . atomically . flip modifyTVar f 

userCheckInHandle :: (HasBreezeApp b) => Handler b v ()
userCheckInHandle = withTop breezeLens $ runAesonApi $ do
  persons <- fromBody
  withTVarWrite checkInGroupCounter (+1)
  gid <- withTVarRead checkInGroupCounter id
  notCheckedIn <- withTVarRead personDB $ toList . getEQ CheckedOut . (@+ (_pid <$> persons))
  withTVarWrite personDB $ fold $ notCheckedIn & mapped %~ updatePerson (set checkedIn $ WaitingApproval gid)
  return $ toDigits gid
  where
    toDigits :: Int -> Int
    toDigits = undefined

approveCheckinHandle :: (HasBreezeApp b) => Handler b v ()
approveCheckinHandle = withTop breezeLens $ runAesonApi $ do
  gid <- fromParam "groupid"
  eid <- use eventId
  toCheckIn <- withTVarRead personDB $ toList . getEQ (WaitingApproval gid)
  vs <- forM toCheckIn (runBreeze' . Checkin eid . _pid)
  case (allOf folded id vs) of
    True -> do
      withTVarWrite personDB $ fold $ toCheckIn & mapped %~ updatePerson (set checkedIn CheckedIn)
      return True
    False -> throwM $ BreezeException $ "Failed to check in group: " ++ (show gid)

initEvent :: Breeze -> IO (Either Text Breeze)
initEvent config = runExceptT $ do
  es <- getEs
  eid <- maybe (throwE "Couldn't fetch event id") return $ es^. nth 0 . key "id"
  ename <- maybe (throwE "Couldn't fetch event name") return $ es^. nth 0 . key "name"
  let conf = config & eventId .~ eid & eventName .~ ename
  getAttendance' conf
  return conf
  where
    getEs :: ExceptT Text IO (Maybe Value)
    getEs = withExceptT (const "Failed to fetch event list") $ do
      now <- liftIO getCurrentTime
      let s = formatTime defaultTimeLocale "%F" (now {utctDay = addDays (-1) (utctDay now)})
      let e = formatTime defaultTimeLocale "%F" now
      runApiReq config "/events" 
          [("start", Just . Char8.pack $ s)
          , ("end", Just . Char8.pack $ e)
          ]
 
listAttendanceHandle :: (HasBreezeApp b) => Handler b v ()
listAttendanceHandle = withTop breezeLens $ runAesonApi $ do
  use breeze >>= getAttendance'
  withTVarRead personDB $ toList

initBreeze :: (HasBreezeApp b) => SnapletInit b Breeze
initBreeze = makeSnaplet "breeze checkin" "a breeze chms mobile friendly checkin system" Nothing $ do
  addRoutes 
    [ ("findperson", getPersonsHandle)
    , ("checkin", userCheckInHandle)
    , ("attendance", listAttendanceHandle)
    ] 
  pdb <- liftIO $ newTVarIO empty
  gcntr <- liftIO $ newTVarIO 0
  addPostInitHook initEvent
  return $ Breeze 
    { _apiKey = "e6e14e8a7e79bb7c62173b9879bacaee"
    , _apiUrl = "https://mountainviewmarietta.breezechms.com/api"
    , _eventId = ""
    , _eventName = ""
    , _personDB = pdb
    , _checkInGroupCounter = gcntr
    }
