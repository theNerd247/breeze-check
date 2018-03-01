{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Version where

import Simple (Error(..), HasApi)
import Simple.String (fromParam, skipParse)
import Control.Exception.Base (throwIO, Exception)
import GHC.Generics
import Data.Data
import Control.Lens
import Control.Monad.Catch (throwM, handle)
import Control.Monad.State

type Version = String

class HasVersion a where
  apiVersion :: Lens' a Version

data WrongApiVersionException = WrongApiVersionException
  { _requiredVersion :: Version
  , _actualVersion :: Version
  }
  deriving (Show, Eq, Ord)

makeLenses ''WrongApiVersionException

instance Exception WrongApiVersionException

checkApiVersion :: (HasApi m, MonadState s m, HasVersion s) => m ()
checkApiVersion = handle handleParseError $ do
  x <- fmap skipParse $ fromParam "api"
  use apiVersion >>= \v -> 
      if v == (x :: String) then
        return ()
      else
        throwM $ WrongApiVersionException v x
  where
    handleParseError (ParamError e) = throwM $ WrongApiVersionException "" ""
