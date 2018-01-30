{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Breeze
import Control.Lens hiding ((.=))
import Control.Monad.Catch (catchAll)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.Breeze
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Data
import Data.Default
import Data.Maybe (listToMaybe)
import Data.Proxy
import Data.Text (pack, unpack)
import Elm
import Network.HTTP.Simple hiding (Proxy)
import Simple.Aeson (runAesonApi, fromBody)
import Simple.Snap
import Simple.String (fromParam, skipParse)
import Snap
import Snap.Snaplet.FastLogger
import qualified Data.ByteString.Char8 as Char8
import qualified Simple as Simple

data App = App
  { _breezeApp :: Breeze 
  , _fastLogger :: Snaplet Logger
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
  addRoutes [("findperson", allowAny >> getPersonsHandle)] 
  wrapSite logAllErrors
  return $ App 
    { _breezeApp = defaultBreezeConfig
    , _fastLogger = lgr
    }

allowAny :: Handler b v ()
allowAny = modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"

logAllErrors f = f `catchAll` (writeLogger Snap.Snaplet.FastLogger.Error . show)

checkInHandle :: (HasBreeze v) => Handler b v ()
checkInHandle = runAesonApi $ return ()

getPersonsHandle :: (HasFastLogger b, HasBreeze v) => Handler b v ()
getPersonsHandle = runAesonApi $ do 
  lname <- skipParse <$> fromParam "lastname"
  persons <- runBreezeWithLog Info $ FindPeople lname Nothing
  return persons

addPersonHandle :: (HasBreeze v, HasFastLogger b) => Handler b v ()
addPersonHandle = runAesonApi $ do
  newPersonInfo <- fromBody
  person <- runBreezeWithLog Info (newPersonInfo :: NewPerson)
  return person

runBreezeWithLog prio f = do
  (req, resp) <- runBreeze f
  writeLogger prio $ req ++ ("Response: {" ++ (show resp) ++ "}")
  return resp

spec = Spec ["Data"] $
  [ "import Json.Decode exposing (..)"
  , "import Json.Decode.Pipeline exposing (..)"
  ] 
  ++ makeElm (Proxy :: Proxy Person)
  where
    makeElm p = 
      [ toElmTypeSourceWith ops p
      , toElmDecoderSourceWith ops p
      ]
    ops = Elm.defaultOptions 
      { fieldLabelModifier = pack . removeUnderscorePrefix . unpack
      }

main = do 
  putStrLn "Generating Elm defs"
  specsToDir [spec] "../breeze-login-ui"
  serveSnaplet defaultConfig appInit
