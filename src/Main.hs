{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Control.Lens hiding ((.=))
import Control.Monad.Catch (catch)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.Breeze
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Data
import qualified Data.List as List
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
import Snap.Snaplet.Breeze
import Snap.Snaplet.FastLogger
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import qualified Data.ByteString.Char8 as Char8
import qualified Simple as Simple

data App = App
  { _breezeApp :: Breeze 
  , _fastLogger :: Snaplet Logger
  , _heist :: Snaplet (Heist App)
  }

makeLenses ''App

instance HasFastLogger App where
  logger = fastLogger

instance HasBreeze App where
  breeze = breezeApp

instance HasHeist App where
  heistLens = subSnaplet heist 

appInit :: SnapletInit App App
appInit = makeSnaplet "breeze-login" "a breeze login web app" Nothing $ do
  lgr <- nestSnaplet "" fastLogger $ initFastLoggerSnaplet (LogFileNoRotate "/tmp/breeze.log" 1000)
  h <- nestSnaplet "" heist $ heistInit "templates"
  addRoutes 
    [ ("", heistServe)
    , ("ui", serveDirectory "ui")
    , ("findperson", getPersonsHandle)
    ] 
  wrapSite $ \s -> do 
    allowAny
    s
    `catch` handleBreezeException
  return $ App 
    { _breezeApp = defaultBreezeConfig
    , _fastLogger = lgr
    , _heist = h
    }

allowAny :: Handler b v ()
allowAny = modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"

{-logAllErrors f = f `catch` (writeLogger Snap.Snaplet.FastLogger.Error . show)-}

handleBreezeException :: BreezeException -> Handler b v ()
handleBreezeException = runAesonApi . return

checkInHandle :: (HasBreeze v) => Handler b v ()
checkInHandle = runAesonApi $ return ()

getPersonsHandle :: (HasFastLogger b, HasBreeze v) => Handler b v ()
getPersonsHandle = runAesonApi $ do 
  lname <- skipParse <$> fromParam "lastname"
  writeLogger Info $ "Last Name: " ++ lname
  persons <- runBreeze $ FindPeople lname Nothing
  return $ filter (\p -> List.isPrefixOf lname $ p^.lastName ) persons

addPersonHandle :: (HasBreeze v, HasFastLogger b) => Handler b v ()
addPersonHandle = runAesonApi $ do
  newPersonInfo <- fromBody
  person <- runBreeze (newPersonInfo :: NewPerson)
  return person

spec = Spec ["Data"] $
  [ "import Json.Decode exposing (..)"
  , "import Json.Decode.Pipeline exposing (..)"
  ] 
  ++ makeElm (Proxy :: Proxy Person)
  ++ makeElm (Proxy :: Proxy BreezeException)
  ++ makeElm (Proxy :: Proxy CheckInGroup)
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
  specsToDir [spec] "./ui/"
  serveSnaplet defaultConfig appInit
