{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Control.Exception.Base (SomeException)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (catch)
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
import FastLogger
import Network.HTTP.Simple hiding (Proxy)
import Simple.Aeson
import Snap
import Snap.Snaplet.Breeze
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import qualified Data.ByteString.Char8 as Char8
import qualified Simple as Simple

data App = App
  { _breezeApp :: Snaplet Breeze 
  , _heist :: Snaplet (Heist App)
  }

makeLenses ''App

instance HasBreezeApp App where
  breezeLens = breezeApp

instance HasHeist App where
  heistLens = subSnaplet heist 

appInit :: SnapletInit App App
appInit = makeSnaplet "breeze-login" "a breeze login web app" Nothing $ do
  b <- nestSnaplet "" breezeApp initBreeze
  h <- nestSnaplet "" heist $ heistInit "templates"
  addRoutes  
    [ ("", heistServe)
    , ("js", serveDirectory "js")
    ]
  wrapSite $ \s -> do 
    allowAny
    s
  return $ App 
    { _breezeApp = b
    , _heist = h
    }

allowAny :: Handler b v ()
allowAny = modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"

{-logAllErrors f = f `catch` (writeLogger Snap.Snaplet.FastLogger.Error . show)-}

spec = Spec ["Data"] $
  [ "import Json.Decode exposing (..)"
  , "import Json.Decode.Pipeline exposing (..)"
  ] 
  ++ makeElm (Proxy :: Proxy Person)
  ++ makeElm (Proxy :: Proxy BreezeException)
  where
    makeElm p = 
      [ toElmTypeSourceWith ops p
      , toElmDecoderSourceWith ops p
      ]
    ops = Elm.defaultOptions 
      { Elm.fieldLabelModifier = pack . removeUnderscorePrefix . unpack
      }


handleServerErrors :: Logger -> SomeException -> Snap ()
handleServerErrors lgger e = do 
  rq <- getRequest
  liftIO $ lgger FastLogger.Error $ 
    (show rq)
    ++ "\n" ++ (show e)
  writeLBS 
    . encode 
    $ BreezeException "An error occured in the server! Try again in a few minutes"

main = do 
  (l, clnLogger) <- liftIO $ initFastLogger $ LogStderr 1024
  serveSnaplet 
    (setErrorHandler (handleServerErrors l) mempty) 
    appInit
  clnLogger
