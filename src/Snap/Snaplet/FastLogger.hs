{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.FastLogger 
  ( initFastLoggerSnaplet
  , writeLogger
  , LogType (..)
  , Logger
  , LogPriority (..)
  , HasFastLogger (..)
  )
    where

import System.Log.FastLogger
import System.Log.FastLogger.Date
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.ByteString.Char8 (split, pack, ByteString)
import Data.Monoid ((<>))
import Snap.Snaplet

data LogPriority = Info | Warn | Error
  deriving (Show)

type Logger = LogPriority -> LogStr -> IO ()

class HasFastLogger m where
  logger :: SnapletLens m Logger

initFastLoggerSnaplet :: LogType -> SnapletInit b Logger
initFastLoggerSnaplet cfg = makeSnaplet 
  "fast-logger" 
  "snaplet for fast-logger library" 
  Nothing $ do
    tc <- liftIO $ newTimeCache "%Y-%m-%d:%T%z"
    (lgr, cleanup) <- liftIO $ newTimedFastLogger tc cfg
    onUnload cleanup
    return $ \p m -> lgr $ \t -> toLogStr . mconcat . fmap (prefix p t) . (split '\n') . fromLogStr $ m

prefix p t m = "\n[" <> t <> "][" <> prio <> "] - " <> m
  where
    prio = pack . show $ p

writeLogger prio msg = withTop logger (ask >>= \f -> liftIO $ f prio (toLogStr msg))
