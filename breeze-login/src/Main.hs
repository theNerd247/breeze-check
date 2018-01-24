{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8 (toStrict)
import Control.Monad.Catch (catchAll)
import Data.Data
import Data.Default
import Breeze
import Data.Breeze
import Data.Maybe (listToMaybe)
import Network.HTTP.Simple
import Simple.Aeson (runAesonApi, fromBody)
import Simple.Snap
import Simple.String (fromParam, skipParse)
import Snap
import Snap.Snaplet.FastLogger
import qualified Data.ByteString.Char8 as Char8
import qualified Simple as Simple

data App = App
  { _breezeApp :: Breeze 
  , _fastLogger :: Snaplet FastLogger
  }

makeLenses ''App

instance HasFastLogger App where
  logger = fastLogger

instance HasBreeze App where
  breeze = breezeApp

defaultBreezeConfig :: Breeze
defaultBreezeConfig = def
  & apiKey .~ "e6e14e8a7e79bb7c62173b9879bacaee"
  & apiUrl .~ "https://mountainviewmarietta.breezechms.com/api"
  & eventId .~ "36862980"

appInit :: SnapletInit App App
appInit = makeSnaplet "breeze-login" "a breeze login web app" Nothing $ do
  lgr <- nestSnaplet "" fastLogger $ initFastLoggerSnaplet (LogFileNoRotate "/tmp/breeze.log" 1000)
  addRoutes [("findperson", getPersonsHandle)] 
  wrapSite logAllErrors
  return $ App 
    { _breezeApp = defaultBreezeConfig
    , _fastLogger = lgr
    }

logAllErrors f = f `catchAll` (writeLogger . show)

checkInHandle :: (HasBreeze v) => Handler b v ()
checkInHandle = runAesonApi $ return ()

getPersonsHandle :: (HasFastLogger b, HasBreeze v) => Handler b v ()
getPersonsHandle = runAesonApi $ do 
  lname <- skipParse <$> fromParam "lastname"
  persons <- runBreezeWithLog $ FindPeople lname Nothing
  return persons

addPersonHandle :: (HasBreeze v, HasFastLogger b) => Handler b v ()
addPersonHandle = runAesonApi $ do
  newPersonInfo <- fromBody
  person <- runBreezeWithLog (newPersonInfo :: NewPerson)
  return person

runBreezeWithLog f = do
  (req, resp) <- runBreeze f
  writeLogger $ req ++ ("Response: {" ++ (show resp) ++ "}")
  return resp

main = serveSnaplet defaultConfig appInit
