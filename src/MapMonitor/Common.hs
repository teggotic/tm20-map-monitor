{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module MapMonitor.Common (
  AppSettingsS3 (..),
  AppSettings (..),
  HasAppSettings (..),
  HasState (..),
  HasUnbeatenAtsCache (..),
  HasBeatenAtsCache (..),
  HasBeatenMapPings (..),
  HasCheckMapFileQueue (..),
  HasS3Connection (..),
  queryAcid,
  updateAcid,
  withAcid1,
  withAcid2,
  settings_auth,
)
where

import Control.Lens
import Data.Acid
import Data.Acid.Advanced
import Data.Time
import Dhall
import MapMonitor.CachedAPIResponses
import MapMonitor.DB
import Protolude
import Servant.Client
import UnliftIO.STM
import UnliftIO.Retry
import PingRPC
import Network.Minio (MinioConn)

data AppSettingsS3
  = AppSettingsS3
  { _s3_creds_access :: !Text
  , _s3_creds_secret :: !Text
  , _s3_creds_host :: !Text
  , _s3_creds_bucket :: !Text
  }
  deriving (Generic, Show)

instance FromDhall AppSettingsS3

data AppSettings
  = AppSettings
  { _settings_auth :: !Text
  , _settings_openplanetAuthSecret :: !Text
  , _settings_mapsCacheDirectory :: !(Maybe FilePath)
  , _settings_logFile :: !(Maybe FilePath)
  , _settings_s3_creds :: !AppSettingsS3
  -- , _settings_static :: !Text
  }
  deriving (Generic, Show)

instance FromDhall AppSettings

$(makeLenses ''AppSettings)

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings

class HasState a where
  stateL :: Lens' a (AcidState MapMonitorState)

class HasUnbeatenAtsCache env where
  unbeatenAtsCacheL :: Lens' env (TVar UnbeatenAtsResponse)

class HasBeatenAtsCache env where
  beatenAtsCacheL :: Lens' env (TVar RecentlyBeatenAtsResponse)

class HasBeatenMapPings env where
  beatenMapPingsL :: Lens' env (TQueue PingRPCMessage)

class HasCheckMapFileQueue env where
  checkMapFileQueueL :: Lens' env (TQueue TMMap)

class HasS3Connection env where
  s3ConnL :: Lens' env MinioConn
  s3BucketL :: Lens' env Text

queryAcid q =
  view stateL >>= flip query' q

updateAcid u =
  view stateL >>= flip update' u

withAcid1 :: (MonadReader env m, HasState env) => (AcidState MapMonitorState -> a -> m x) -> a -> m x
withAcid1 m a = do
  acid <- view stateL
  m acid a

withAcid2 :: (MonadReader env m, HasState env) => (AcidState MapMonitorState -> a -> b -> m x) -> a -> b -> m x
withAcid2 m a b = do
  acid <- view stateL
  m acid a b
