{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Sync where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.IxSet
import Data.Lens
import Data.Time
import Snap

data SyncTime = SyncCloud UTCTime 
              | SyncLocal UTCTime 
    deriving (Eq, Ord, Show)

data SyncConfig m f a = SyncConfig
  { _syncInterval :: DiffTime
  , _syncTo :: f a -> m ()
  , _syncFrom :: m (f a)
  }

makeClassy ''SyncConfig

data Sync m f a = Sync
  { _lastSynced :: SyncTime
  , _syncData :: f a
  , _synConfig :: SyncConfig m f
  }

makeLenses ''Sync

class HasSync a where
  sync :: SnapletLens a Sync

initSyncSnaplet :: SyncConfig -> SnapletInit b Sync
initSyncSnaplet config = makeSnaplet 
  "sync" 
  "snaplet for syncing data between server"
  Nothing $ do
    now <- liftIO getCurrentTime
    return $ Sync
      { _sync = now
      }

syncDataFromLocal :: (MonadIO m, Traversable f) => f a -> Sync m f a -> m (Sync m f a)
syncDataFromLocal d s = syncData' (s^.lastSynced)
  where
    syncData' (SyncCloud t) = do
      let d' = syncPrio s^.syncData d
      s^.synConfig.syncTo d'
      now <- liftIO getCurrentTime
      return $ s 
        & lastSynced .~ SyncLocal now
        & syncData .~ d'
    syncData' (SyncLocal t) = do
      d' <- s^.synConfig.syncFrom
      s^.synConfig.syncTo d
      now <- liftIO getCurrentTime
      return $ s 
        & lastSynced .~ SyncCloud now
        & syncData .~ d

syncDataFromCloud :: (MonadIO m, Traversable f) => Sync m f a -> m (Sync m f a)
syncDataFromCloud s = syncData' (s^.lastSynced)
  where
    syncData' (SyncCloud t) = do
      d <- s^.synConfig.syncFrom
      now <- liftIO getCurrentTime
      return $ s' 
        & lastSynced .~ SyncCloud now
        & syncData .~ d
    syncData' (SyncLocal t) = do
      d <- s^.synConfig.syncFrom
      let d' = syncPrio d s^.syncData
      now <- liftIO getCurrentTime
      return $ s 
        & lastSynced .~ SyncCloud now
        & syncData .~ d'

syncPrio :: f a -> fa -> f a
