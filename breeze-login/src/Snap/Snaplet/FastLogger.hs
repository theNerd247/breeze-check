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
import System.Log.FastLogger.Date
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Monoid ((<>))
import Snap.Snaplet

class HasFastLogger m where
  logger :: SnapletLens m FastLogger

initFastLoggerSnaplet :: LogType -> SnapletInit b FastLogger
initFastLoggerSnaplet cfg = makeSnaplet 
  "fast-logger" 
  "snaplet for fast-logger library" 
  Nothing $ do
    liftIO $ clearLog cfg
    tc <- liftIO $ newTimeCache "%Y-%b-%d:%T %z"
    (lgr, cleanup) <- liftIO $ newTimedFastLogger tc cfg
    onUnload cleanup
    return $ \m -> lgr (\t -> toLogStr ("\n[" <> t <> "]: ") <> m)
    where
      clearLog (LogFileNoRotate f _) = writeFile f ""
      clearLog (LogFile (FileLogSpec f _ _) _) = writeFile f ""

writeLogger msg = withTop logger (ask >>= \f -> liftIO . f . toLogStr $ msg)
