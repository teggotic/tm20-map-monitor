{-# LANGUAGE DeriveGeneric #-}

module MapMonitor.Common (
  AppSettings (..),
  HasAppSettings (..),
  HasState (..),
  HasUnbeatenAtsCache (..),
  HasBeatenAtsCache (..),
  queryAcid,
  updateAcid,
  withAcid1,
  withAcid2,
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

data AppSettings
  = AppSettings
  { _settings_auth :: Text
  , _settings_openplanetAuthSecret :: Text
  , _settings_mapsCacheDirectory :: Maybe FilePath
  }
  deriving (Generic, Show)

instance FromDhall AppSettings

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings

class HasState a where
  stateL :: Lens' a (AcidState MapMonitorState)

class HasUnbeatenAtsCache env where
  unbeatenAtsCacheL :: Lens' env (TVar UnbeatenAtsResponse)

class HasBeatenAtsCache env where
  beatenAtsCacheL :: Lens' env (TVar RecentlyBeatenAtsResponse)

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
