{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module FastLogger 
  ( initFastLogger
  , LogType (..)
  , Logger
  , LogPriority (..)
  )
  where

import System.Log.FastLogger
import System.Log.FastLogger.Date
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Data
import GHC.Generics
import Data.ByteString.Char8 (split, pack, ByteString)
import Data.Monoid ((<>))

data LogPriority = Info | Warn | Error
  deriving (Show, Data, Generic)

type Logger = LogPriority -> String -> IO ()

initFastLogger :: (MonadIO m) => LogType -> m (Logger, IO ())
initFastLogger cfg = do
  tc <- liftIO $ newTimeCache "%Y-%m-%d:%T%z"
  (lgr, cleanup) <- liftIO $ newTimedFastLogger tc cfg
  return ((\prio msg -> liftIO $ (mkLogger lgr) prio (toLogStr msg)) , cleanup)
  
mkLogger lgr p m = lgr $ \t -> toLogStr . splitLines t . fromLogStr $ m
  where
    splitLines t = mconcat . fmap (prefix p t) . (split '\n')

prefix p t m = "\n[" <> t <> "][" <> prio <> "] - " <> m
  where
    prio = pack . show $ p
