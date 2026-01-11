{-# LANGUAGE DeriveGeneric #-}

module MapMonitor.Common (
  NadeoTokenState (..),
  AppSettings (..),
  HasAppSettings (..),
  HasState (..),
  HasNadeoCoreClient (..),
  HasXertrovClient (..),
  HasNadeoLiveClient (..),
  HasTMXClient (..),
  HasOpenPlanetClient (..),
  HasNadeoTokenState (..),
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

data NadeoTokenState
  = NadeoTokenState
  { _nadeoTokenState_accessToken :: Text
  , _nadeoTokenState_accessExpires :: UTCTime
  , _nadeoTokenState_refreshToken :: Text
  , _nadeoTokenState_refreshExpires :: UTCTime
  }

data AppSettings
  = AppSettings
  { _settings_auth :: Text
  , _settings_openplanetAuthSecret :: Text
  }
  deriving (Generic, Show)

instance FromDhall AppSettings

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings

class HasState a where
  stateL :: Lens' a (AcidState MapMonitorState)

class HasNadeoCoreClient env where
  nadeoCoreClientL :: Lens' env ClientEnv

class HasXertrovClient env where
  xertrovClientL :: Lens' env ClientEnv

class HasNadeoLiveClient env where
  nadeoLiveClientL :: Lens' env ClientEnv

class HasTMXClient env where
  tmxClientL :: Lens' env ClientEnv

class HasOpenPlanetClient env where
  openPlanetClientL :: Lens' env ClientEnv

class HasNadeoTokenState env where
  nadeoTokenStateL :: Lens' env (TMVar (Maybe NadeoTokenState))

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
