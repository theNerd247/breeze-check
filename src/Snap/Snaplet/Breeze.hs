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
import Data.Monoid ((<>), Endo(..))
import Data.IxSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List (isPrefixOf)
import Data.Proxy
import Data.Text (Text)
import Data.Time
import FastLogger
import Data.String (fromString)
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
    , ("details",     Just "true")
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

instance BreezeApi GetEvents where
  type BreezeResponse GetEvents = NonEmpty EventInfo
  runBreeze b _ = do
    now <- liftIO getZonedTime
    let s = formatTime defaultTimeLocale "%F" now --{utctDayTime = 0}
    let e = formatTime defaultTimeLocale "%F" now --{utctDayTime = 86399}
    runApiReq b "/events" 
      [ ("start", Just . Char8.pack $ s)
      , ("end", Just . Char8.pack $ e)
      ]


{-runApiReq :: (HasBreeze s, MonadThrow m, MonadIO m, FromJSON b) => s -> [Char] -> [(Char8.ByteString, Maybe Char8.ByteString)] -> m b-}
runApiReq config path params = do
  let modReq = HTTP.addRequestHeader "Content-Type" "application/json"
            . HTTP.addRequestHeader "Api-Key" (Char8.pack $ config^.apiKey)
            . HTTP.setRequestQueryString params
  req <- fmap modReq $ HTTP.parseRequest ((config^.apiUrl) ++ (addRoot path))
  res <- HTTP.getResponseBody <$> HTTP.httpJSONEither req
  let ltype = either (const Error) (const Info) res
  liftIO $ (config^.logger) ltype (show req <> "\n" <> (show res))
  either throwM return res 
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
  withTVarWrite personDB $ appEndo . fold $
    eas^..folded.to attendingPerson & mapped %~ Endo . updateAttending
  where
    updateAttending :: Person -> IxSet Person -> IxSet Person
    updateAttending p db = maybe
      (insert p db)
      (\x -> updateIx (x^.pid) (x & checkedIn .~ max (x^.checkedIn) (p^.checkedIn)) db)
      (getOne $ db @= (p^.pid))
    
getPersonsHandle :: (HasBreezeApp b) => Handler b v ()
getPersonsHandle = withTop breezeLens $ runAesonApi $ do 
  lname <- skipParse <$> fromParam "lastname"
  ps <- runBreeze' $ FindPeople lname Nothing
  let persons = ps^..folded.filtered (isPrefixOf lname . (view lastName))
  withTVarWrite personDB $ addMissingPersons persons
  withTVarRead personDB $ toList . getEQ CheckedOut . (@+ (persons^..folded.pid))
    where
      addMissingPersons ps = appEndo $ foldMap (Endo . addMissingPerson) ps
      addMissingPerson p db = maybe
        (insert p db)
        (return db)
        (getOne $ db @= (p^.pid))

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
  notCheckedIn <- withTVarRead personDB $ toList . getEQ CheckedOut . (@+ (persons :: [PersonId]))
  case notCheckedIn of
    [] -> do
      cgid <- withTVarRead checkInGroupCounter id
      waiting <- withTVarRead personDB $ toList . (@>=<= (0, cgid)) . (@+ (persons :: [PersonId])) 
      let mid = firstOf traverse waiting ^? _Just . checkedIn . _WaitingApproval
      maybe 
        (throwM $ BreezeException $ "Could not find perons to login: " <> (show persons)) 
        (return) 
        mid
    _ -> do
      withTVarWrite checkInGroupCounter (+1)
      gid <- withTVarRead checkInGroupCounter id
      breezeLog Info $ "Creating log group " <> (show gid) <> (fold $ notCheckedIn^..folded.to show.to (<>"\n"))
      withTVarWrite personDB $ 
        notCheckedIn
          ^..folded
            .to (updatePerson $ set checkedIn $ WaitingApproval gid)
            .to Endo
          ^.folded
          ^.to appEndo
      ps <- withTVarRead personDB $ toList . (@+ persons)
      breezeLog Info $ foldMapOf folded show ps
      return gid

getCheckInGroupHandle :: (HasBreezeApp b) => Handler b v ()
getCheckInGroupHandle = withTop breezeLens $ runAesonApi $ do
  gid <- fromParam "groupid"
  withTVarRead personDB $ toList . getEQ (WaitingApproval gid)

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

breezeLog p m = use logger >>= \f -> liftIO $ f p m

initEvent :: Breeze -> IO (Either Text Breeze)
initEvent config = runExceptT $ do
  conf <- case (config^.debug) of
    True -> return $ config & eventId .~ "40532683" & eventName .~ "Staff Meeting"
    _ -> do 
      es <- runBreeze config GetEvents
      return $ config & eventId .~ (es^?! traverse.eid) & eventName .~ (es^?! traverse.ename)
  getAttendance' conf
  return conf
  `catch` handleBreeze `catch` handleHTTP
  where
    handleBreeze :: BreezeException -> ExceptT Text IO a
    handleBreeze = throwE . fromString . show

    handleHTTP :: HTTP.JSONException -> ExceptT Text IO a
    handleHTTP = throwE . fromString . show
 
listAttendanceHandle :: (HasBreezeApp b) => Handler b v ()
listAttendanceHandle = withTop breezeLens $ runAesonApi $ 
  withTVarRead personDB toList

eventInfoHandle :: (HasBreezeApp b) => Handler b v ()
eventInfoHandle = withTop breezeLens $ runAesonApi $ do
  eid <- use eventId
  ename <- use eventName
  return $ object [("event-id", fromString eid), ("event-name", fromString ename)]

mkBreeze :: (MonadIO m) => FilePath -> m Breeze
mkBreeze fp = do
  pdb <- liftIO $ newTVarIO empty
  gcntr <- liftIO $ newTVarIO 0
  (lgr, cleanup) <- liftIO $ initFastLogger (LogFileNoRotate (fp <> "/breeze.log") 1024)
  return $ Breeze 
    { _apiKey = "e6e14e8a7e79bb7c62173b9879bacaee"
    , _apiUrl = "https://mountainviewmarietta.breezechms.com/api"
    , _eventId = ""
    , _eventName = ""
    , _personDB = pdb
    , _logger = lgr
    , _loggerCleanup = cleanup
    , _checkInGroupCounter = gcntr
    , _debug = True
    }

initBreeze :: (HasBreezeApp b) => SnapletInit b Breeze
initBreeze = makeSnaplet "breeze" "a breeze chms mobile friendly checkin system" (Just $ return "breeze") $ do
  addRoutes 
    [ ("findperson", getPersonsHandle)
    , ("checkin", userCheckInHandle)
    , ("attendance", listAttendanceHandle)
    , ("eventinfo", eventInfoHandle)
    , ("getgroup", getCheckInGroupHandle)
    , ("approve", approveCheckinHandle) 
    ] 
  addPostInitHook initEvent
  b <- getSnapletFilePath >>= mkBreeze  
  onUnload (b^.loggerCleanup)
  return b
