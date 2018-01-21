{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.FastLogger 
  (initFastLoggerSnaplet
  , writeLogger
  , FastLogger
  , LogType (..)
  , HasFastLogger (..)
  )
    where

import System.Log.FastLogger
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Snap.Snaplet

class HasFastLogger m where
  logger :: SnapletLens m FastLogger

initFastLoggerSnaplet :: LogType -> SnapletInit b FastLogger
initFastLoggerSnaplet cfg = makeSnaplet 
  "fast-logger" 
  "snaplet for fast-logger library" 
  Nothing $ do
    (lgr, cleanup) <- liftIO $ newFastLogger cfg
    onUnload cleanup
    return lgr

writeLogger msg = withTop logger (ask >>= \f -> liftIO . f . toLogStr $ msg)
