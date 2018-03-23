{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Snap.Snaplet.Breeze where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Lens hiding ((.=))
import Control.Applicative ((<|>))
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Except
import Control.Monad (forM)
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.Breeze
import Control.Lens.Extras (is)
import Data.ByteString.Lazy.Char8 (toStrict, fromStrict)
import Data.Default
import Data.Foldable (fold)
import Data.Monoid ((<>), Endo(..))
import Data.IxSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List (isPrefixOf, (\\))
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
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Network.HTTP.Simple as HTTP
import qualified Data.List as List

data FindPeople = FindPeople LastName (Maybe Address)
data GetAttendance = GetAttendance EventId
data Checkin = Checkin EventId Person 
data MakeNewPerson = MakeNewPerson Person
data GetEvents = GetEvents
data GetFields = GetFields

class HasBreezeApp b where
  breezeLens :: SnapletLens b Breeze

class BreezeApi a where
  type BreezeResponse a :: *
  runBreeze :: (MonadIO m, MonadThrow m) => Breeze -> a -> m (BreezeResponse a)

instance BreezeApi FindPeople where
  type BreezeResponse FindPeople = [Person]
  runBreeze b (FindPeople lname maddr) = do 
    ps <- runApiReq b "/people" $ 
      [ ("details", Just . Char8.pack . show $ 0)
      , ("filter_json", Just . toStrict . encode $ object
          [ "189467778_last"   .= lname
          {-, "697961327_street" .= (maddr^.street)-}
          {-, "697961327_city"   .= (maddr^.city)-}
          {-, "697961327_state"  .= (maddr^.state)-}
          {-, "697961327_zip"    .= (maddr^.zipcode)-}
          ])
      ]
    return $ (ps :: [BreezePerson])^..folded.bPerson

instance BreezeApi GetAttendance where
  type BreezeResponse GetAttendance = [Person]
  runBreeze b (GetAttendance eid) = do 
    ps <- runApiReq b "/events/attendance/list"
      [ ("instance_id", Just . encodeUtf8 $ eid)
      , ("details",     Just "true")
      , ("type",        Just "person")
      ]
    return $ (ps :: [ParseAttendance])^..folded.attendingPerson
    
instance BreezeApi Checkin where
  type BreezeResponse Checkin = Person
  runBreeze b (Checkin eid p) = 
    case p^.checkedIn of
      WaitingApproval _ -> chkIn p
      WaitingCreation _ _ -> do
        runBreeze b (MakeNewPerson p)
          >>= chkIn
      _ -> return p
    where
      chkIn p = do 
        r <- runApiReq b "/events/attendance/add"
          [ ("person_id"   , Just . Char8.pack . show $ p^.pid)
          , ("instance_id" , Just . encodeUtf8 $ eid)
          , ("direction"   , Just "in")
          ]
        return $ if r then (p & checkedIn .~ CheckedIn) else p

instance BreezeApi MakeNewPerson where
  type BreezeResponse MakeNewPerson = Person
  runBreeze b (MakeNewPerson np) = 
    case np^? newPersonInfo . _Just . fullyNew of
      Just True -> do
        ps <- runApiReq b "/people/add" $ 
          [ ("first"       , Just . encodeUtf8 $ np^.firstName)
          , ("last"        , Just . encodeUtf8 $ np^.lastName)
          ] ++ personFields
        return $ (ps :: BreezePerson)^.bPerson

      Just False -> do
        ps <- runApiReq b "/people/update" $ 
          [ ("person_id"       , Just . Char8.pack . show $ np^.pid)
          ] ++ personFields
        return $ (ps :: BreezePerson)^.bPerson

      _ -> return np

    where

      field :: String -> String -> Value -> Value -> Value
      field i t r d = object
        [ "field_id"       .= i 
        , "field_type"     .= t
        , "response"       .= r
        , "details"        .= d
        ]
  
      personFields = 
        case np^.newPersonInfo of
          Nothing -> []
          (Just a) -> 
            [("fields_json" , Just . toStrict . encode $
              [ field "697961327" "address" (toJSON True) $ object
                  [ "street_address" .= (a^.street)
                  , "city"   .= (a^.city)
                  , "state"  .= (a^.state)
                  , "zip"    .= (a^.zipcode)
                  ]
              , field "2005562485" "email" (toJSON True) $ object
                  [ "address" .= (a^.newEmail) ]
              , field "2105844304" "textarea" (toJSON $ a^.newCurrentChurch) Null
              ])
            ]


instance BreezeApi GetEvents where
  type BreezeResponse GetEvents = [EventInfo]
  runBreeze b _ = do
    now <- liftIO getZonedTime
    let s = formatTime defaultTimeLocale "%F" now --{utctDayTime = 0}
    let e = formatTime defaultTimeLocale "%F" now --{utctDayTime = 86399}
    runApiReq b "/events" 
      [ ("start", Just . Char8.pack $ s)
      , ("end", Just . Char8.pack $ e)
      ]

instance BreezeApi GetFields where
  type BreezeResponse GetFields = Value
  runBreeze b _ = runApiReq b "/profile" []

{-runApiReq :: (HasBreeze s, MonadThrow m, MonadIO m, FromJSON b) => s -> [Char] -> [(Char8.ByteString, Maybe Char8.ByteString)] -> m b-}
runApiReq config path params = do
  let modReq = HTTP.addRequestHeader "Content-Type" "application/json"
            . HTTP.addRequestHeader "Api-Key" (Char8.pack $ config^.apiKey)
            . HTTP.setRequestQueryString params
  req <- fmap modReq $ HTTP.parseRequest ((config^.apiUrl) ++ (addRoot path))
  res <- HTTP.getResponseBody <$> HTTP.httpJSONEither req
  let lgr = either (const $ config^.errLogger) (const $ config^.infoLogger) res
  liftIO $ lgr $ show req <> "\n" <> (show res)
  either throwM return res 
  where
    addRoot xs@('/':_) = xs
    addRoot xs = '/':xs

runBreeze' a = do 
  config <- use breeze
  runBreeze config a

getAttendance' :: (MonadIO m, MonadThrow m) => Breeze -> m ()
getAttendance' config = flip evalStateT config $ do
  eid <- (liftIO . atomically $ readTVar (config^.breezeEventInfo)) >>= return . (view eventId)
  eas <- runBreeze' $ GetAttendance eid
  withTVarWrite personDB $ 
    case eas of
      [] -> (const mempty)
      _ -> appEndo . fold $ eas & mapped %~ Endo . updateAttending
  where
    updateAttending :: Person -> IxSet Person -> IxSet Person
    updateAttending p db = maybe
      (insert p db)
      (\x -> updateIx (x^.pid) (x & checkedIn .~ max (x^.checkedIn) (p^.checkedIn)) db)
      (getOne $ db @= (p^.pid))
    
getPersonsHandle :: (HasBreezeApp b) => Handler b v ()
getPersonsHandle = withTop breezeLens $ runAesonApi $ do 
  lname <- (sanitize . Text.pack . skipParse) <$> fromParam "lastname"
  ps <- runBreeze' $ FindPeople lname Nothing
  let persons = ps^..folded.filtered (Text.isPrefixOf lname . (view $ lastName . to (Text.toLower)))
  withTVarWrite personDB $ addMissingPersons persons
  withTVarRead personDB $ toList . getEQ CheckedOut . (@+ (persons^..folded.pid))
    where
      addMissingPersons ps = appEndo $ foldMap (Endo . addMissingPerson) ps
      addMissingPerson p db = maybe
        (insert p db)
        (return db)
        (getOne $ db @= (p^.pid))

      sanitize = Text.toLower . Text.filter (Char.isAlpha)

updatePerson :: (Person -> Person) -> Person -> IxSet Person -> IxSet Person
updatePerson f p = updateIx (p^.pid) (f p)

withTVarRead :: (MonadState s m, MonadIO m) => Lens' s (TVar a) -> (a -> b) -> m b
withTVarRead l f = use l >>= liftIO . atomically . fmap f . readTVar 

withTVarWrite :: (MonadState s m, MonadIO m) => Lens' s (TVar a) -> (a -> a) -> m ()
withTVarWrite l f = use l >>= liftIO . atomically . flip modifyTVar f 

chainTVarWrite :: (MonadState s m, Monoid a, MonadIO m, Foldable f) => Lens' s (TVar a) -> (x -> a -> a) -> f x -> m () 
chainTVarWrite l f xs = withTVarWrite l $
  xs^..folded
      .to f
      .to Endo
  & appEndo . fold

userCheckInHandle :: (HasBreezeApp b) => Handler b v ()
userCheckInHandle = withTop breezeLens $ runAesonApi $ do
  il <- use infoLogger
  ps <- fromBody
  let forCheckInOnly = (ps :: [Person])^..folded.filtered (\p -> p^?newPersonInfo . _Just . fullyNew . to not ^. non True)
  {-let forUpdateOnly = ps^..folded.filtered (\p -> p^?newPersonInfo . _Just . fullyNew . to not ^. non False)-}
  let newPersons = ps^..folded.filtered (\p -> p^?newPersonInfo . _Just . fullyNew ^. non False)
  withTVarWrite checkInGroupCounter (+1)
  liftIO $ il $ "Checkin recieved: \n" ++ (show ps)
  gid <- withTVarRead checkInGroupCounter id
  liftIO $ il $ "Creating checkin group: " 
          <> (show gid) 
          <> "\n"
          <> (fold $ ps^..folded.to show.to (<>"\n"))
  chainTVarWrite personDB (updatePerson $ set checkedIn $ WaitingApproval gid) forCheckInOnly
  {-chainTVarWrite personDB (updatePerson $ \p -> p & checkedIn .~ WaitingCreation gid (p^.pid)) forUpdateOnly-}
  chainTVarWrite personDB insert $ 
    newPersons
      ^@..folded ^..folded.to (\(i,p) -> p & checkedIn .~ (WaitingCreation gid i))
  return gid

getCheckInGroupHandle :: (HasBreezeApp b) => Handler b v ()
getCheckInGroupHandle = withTop breezeLens $ runAesonApi $ do
  gid <- fromParam "groupid"
  withTVarRead personDB $ toList . getEQ (GID gid)

getCheckInGroupByLastNameHandle :: (HasBreezeApp b) => Handler b v ()
getCheckInGroupByLastNameHandle = withTop breezeLens $ runAesonApi $ do
  lname <- (Text.toLower . Text.pack . skipParse) <$> fromParam "lastName"
  withTVarRead personDB $ 
    (\ps -> 
      ps^..folded.filtered
        ( \p -> 
                p^.lastName.to (Text.toLower).to (Text.isPrefixOf lname)
        )
    )
    . toList 
    . getEQ (HasGID True)



cancelCheckinHandle :: (HasBreezeApp b) => Handler b v ()
cancelCheckinHandle = withTop breezeLens $ runAesonApi $ do
  gid <- fromParam "groupid"
  ps <- withTVarRead personDB $ toList . getEQ (GID gid)
  il <- use infoLogger
  liftIO . il $ "Cancelling Check In For: " ++ (show gid) ++ (mconcat $ (("\n"++) . show) <$> ps)
  chainTVarWrite personDB (updatePerson $ set checkedIn CheckedOut) (ps^..folded.filtered (view $ newPersonInfo . to (isn't _Just)))
  chainTVarWrite personDB (deleteIx . (view newPersonInfo)) (ps^..folded.filtered (view $ newPersonInfo . to (is _Just)))
  return True

approveCheckinHandle :: (HasBreezeApp b) => Handler b v ()
approveCheckinHandle = withTop breezeLens $ runAesonApi $ do
  gid <- fromParam "groupid"
  eid <- withTVarRead breezeEventInfo (^.eventId)
  il <- use infoLogger
  toCheckIn <- withTVarRead personDB $ toList . getEQ (GID gid)
  vs <- forM toCheckIn $ \p -> do
      checkedInPerson <- runBreeze' $ Checkin eid p

      withTVarWrite personDB $ 
        case p^.checkedIn of
          c@(WaitingCreation _ _) -> updateIx c checkedInPerson
          _ -> updateIx (p^.pid) checkedInPerson
      liftIO $ il $ "Logged in: " <> (show checkedInPerson)
      return True
  case vs of
    [] -> throwM $ BreezeException $ "Uh oh! It seems the family of group id: " ++ (show gid) ++ " canceled their check in before you could approve!"
    _ -> return $ allOf folded id vs

resetServer :: (MonadIO m, MonadThrow m) => Breeze -> m Breeze
resetServer config = do
  debugFlag <- liftIO $ atomically $ readTVar $ config^.debug 
  einfo <- case debugFlag of
    True -> return $ def & eventId .~ "40532683" & eventName .~ "Staff Meeting"
    _ -> do 
      es <- runBreeze config GetEvents
      maybe (return def) return $ es^? traverse
  liftIO . atomically $ writeTVar (config^.breezeEventInfo) einfo
  getAttendance' config
  return config

initEvent :: Breeze -> IO (Either Text Breeze)
initEvent config = runExceptT $ 
  resetServer config
  `catch` handleBreeze `catch` handleHTTP
  where
    handleBreeze :: BreezeException -> ExceptT Text IO a
    handleBreeze = throwE . fromString . show

    handleHTTP :: HTTP.JSONException -> ExceptT Text IO a
    handleHTTP = throwE . fromString . show
 
listAttendanceHandle :: (HasBreezeApp b) => Handler b v ()
listAttendanceHandle = withTop breezeLens $
  withTVarRead personDB (("<pre>" ++) . (++ "</pre>") . concat . List.intersperse "\n". fmap show . toList)
    >>= writeLBS . fromStrict . Char8.pack

getEventListHandler :: (HasBreezeApp b) => Handler b v ()
getEventListHandler = withTop breezeLens $ runAesonApi $ do
  db <- withTVarRead debug id
  if db then do
        withTVarRead breezeEventInfo id >>= return . (:[])
  else
    runBreeze' GetEvents

eventInfoHandle :: (HasBreezeApp b) => Handler b v ()
eventInfoHandle = getEvent <|> setEvent
  where 
    getEvent = method GET $ withTop breezeLens $ runAesonApi $ do
      withTVarRead breezeEventInfo id

    setEvent = method POST $ withTop breezeLens $ runAesonApi $ do 
      db <- withTVarRead debug id
      case db of
        True -> withTVarRead breezeEventInfo id
        _ -> do
          eid <- fromBody 
          es <- runBreeze' GetEvents
          let validEvent = es^? traverse.filtered (view $ eventId.to (==eid))
          maybe 
            (throwM $ BreezeException $ "Event with id: " ++ (Text.unpack eid) ++ " doesn't exist")
            (\e -> do 
              withTVarWrite breezeEventInfo $ const e 
              newConfig <- use id >>= resetServer 
              assign id newConfig
              return e
            )
            validEvent

setDebugHandler :: (HasBreezeApp b) => Handler b v ()
setDebugHandler = withTop breezeLens $ runAesonApi $ do
  db <- fromParam "enable"
  withTVarWrite debug (const db)
  newConfig <- use id >>= resetServer
  assign id newConfig
  withTVarRead debug id

mkBreeze :: (MonadIO m) => m Breeze
mkBreeze = do
  pdb <- liftIO $ newTVarIO empty
  einfo <- liftIO $ newTVarIO def
  gcntr <- liftIO $ newTVarIO 0
  debugFlag <- liftIO $ newTVarIO False
  (ilgr, icln) <- liftIO $ initInfoLogger
  (elgr, ecln) <- liftIO $ initErrLogger
  return $ Breeze 
    { _apiKey = "e6e14e8a7e79bb7c62173b9879bacaee"
    , _apiUrl = "https://mountainviewmarietta.breezechms.com/api"
    , _breezeEventInfo = einfo
    , _personDB = pdb
    , _infoLogger = ilgr
    , _errLogger = elgr
    , _infoLoggerCleanup = icln
    , _errLoggerCleanup = ecln
    , _checkInGroupCounter = gcntr
    , _debug = debugFlag
    }


initBreeze :: (HasBreezeApp b) => SnapletInit b Breeze
initBreeze = makeSnaplet "breeze" "a breeze chms mobile friendly checkin system" Nothing $ do
  addRoutes 
    [ ("findperson", getPersonsHandle)
    , ("checkin", userCheckInHandle)
    , ("attendance", listAttendanceHandle)
    , ("eventinfo", eventInfoHandle)
    , ("eventList", getEventListHandler)
    , ("getgroup", getCheckInGroupHandle)
    , ("getgrouplastname", getCheckInGroupByLastNameHandle)
    , ("approve", approveCheckinHandle) 
    , ("cancel", cancelCheckinHandle)
    , ("debug", setDebugHandler)
    ] 
  addPostInitHook initEvent
  b <- mkBreeze  
  wrapSite $ \s -> do
    s `catch` handleBreeze
  onUnload $ do 
    liftIO $ b^.infoLoggerCleanup
    liftIO $ b^.errLoggerCleanup
  return b
  where
    handleBreeze :: (HasBreezeApp b) => BreezeException -> Handler b v ()
    handleBreeze e = withTop breezeLens $ runAesonApi $ do 
      el <- use errLogger
      liftIO $ el $ show e
      return e
