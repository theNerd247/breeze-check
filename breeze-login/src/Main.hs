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
import FastLogger
import Network.HTTP.Simple hiding (Proxy)
import Simple.Aeson
import Snap
import Snap.Snaplet.Breeze
import Snap.Snaplet.Heist.Compiled
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
  return $ App 
    { _breezeApp = b
    , _heist = h
    }

handleServerErrors :: Logger -> SomeException -> Snap ()
handleServerErrors lgger e = do 
  rq <- getRequest
  liftIO $ lgger $ 
    (show rq)
    ++ "\n" ++ (show e)
  writeLBS 
    . encode 
    $ BreezeException "An error occured in the server! Try again in a few minutes"

main = do 
  (l, clnLogger) <- liftIO $ initErrLogger
  let cfg = 
        defaultConfig
            & setSSLBind "0.0.0.0"
            & setSSLPort 4443
            & setSSLChainCert True
            & setErrorHandler (handleServerErrors l)
  serveSnaplet cfg appInit
  clnLogger
    where
