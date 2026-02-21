{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import qualified RIO.Text as T
import Data.Acid
import Data.Acid.Remote (openRemoteState, skipAuthenticationPerform)
import Dhall
import MapMonitor.CachedAPIResponses
import MapMonitor.DB
import MapMonitor.Common
import MapMonitor.Server
import Network.HTTP.Client as NHC (ManagerSettings (managerModifyRequest), Request (requestHeaders), newManager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (hUserAgent)
import Network.Socket (PortNumber)
import Protolude hiding (withFile, atomically, bracket, forkIO, threadDelay, to, toList, try)
import RIO (MonadUnliftIO, newTMVarIO, withLogFunc, setLogUseTime, logOptionsHandle, withFile, hSetBuffering, BufferMode (LineBuffering), LogFunc, IsString (fromString))
import Servant.Auth.Server
import Servant.Client
import UnliftIO.Exception (bracket)
import UnliftIO.STM
import System.Directory (doesFileExist)
import RIO.Time (getCurrentTime)
import PingRPC (withPubSocket)
import Network.Minio
import Control.Lens.TH
import Control.Concurrent.STM.TSem

data CollectCacheState
  = CollectCacheState
  { _ccs_acid :: !(AcidState MapMonitorState)
  , _ccs_settings :: !AppSettings
  }

$(makeLenses ''CollectCacheState)

instance HasState CollectCacheState where
  stateL = ccs_acid

instance HasAppSettings CollectCacheState where
  appSettingsL = ccs_settings

runInApp :: (MonadUnliftIO m) => AcidState MapMonitorState -> TQueue TMMap -> ReaderT AppState m b -> m b
runInApp acid checkMapFileQueue m = do
  settings <- liftIO $ input auto "./settings.dhall"

  unbeatenAtsCache <- flip runReaderT (CollectCacheState acid settings) $ do
    collectUnbeatenAtsResponse >>= liftIO . newTVarIO
  beatenAtsCache <- flip runReaderT acid $ do
    collectBeatenAtsResponse >>= liftIO . newTVarIO

  jwtAccessKey <- liftIO $
    (doesFileExist "/tmp/jwt-access-key.secret") >>= Protolude.bool generateKey (readKey "/tmp/jwt-access-key.secret")
  manager' <-
    liftIO $
      NHC.newManager
        tlsManagerSettings
          { managerModifyRequest = \req -> do
              return req{requestHeaders = requestHeaders req <> [(hUserAgent, "teggot@proton.me; unbeaten-maps-monitor project")]}
          }
  let
    coreNadeoClient = (mkClientEnv manager' (BaseUrl Https "prod.trackmania.core.nadeo.online" 443 ""))
    liveServicesNadeoClient = (mkClientEnv manager' (BaseUrl Https "live-services.trackmania.nadeo.live" 443 ""))
    tmxClient = (mkClientEnv manager' (BaseUrl Https "trackmania.exchange" 443 ""))
    xertrovClient = (mkClientEnv manager' (BaseUrl Https "map-monitor.xk.io" 443 ""))
    openPlanetClient = (mkClientEnv manager' (BaseUrl Https "openplanet.dev" 443 ""))
    _mockClient = (mkClientEnv manager' (BaseUrl Http "localhost" 7249 ""))
    jwtSettings = defaultJWTSettings jwtAccessKey
    c =
      setCreds
      (CredentialValue (AccessKey $ _s3_creds_access $ _settings_s3_creds settings) (fromString $ T.unpack $ _s3_creds_secret $ _settings_s3_creds settings) Nothing)
      (fromString $ T.unpack $ "https://" <> (_s3_creds_host $ _settings_s3_creds settings))
  conn <- liftIO $ mkMinioConn c manager'
  validationSem <- atomically $ newTSem 2
  let
    go h = do
      throttler <- newTMVarIO =<< getCurrentTime
      tokenStateRef <- RIO.newTMVarIO Nothing

      logOptions <- setLogUseTime True <$> logOptionsHandle h False
      withLogFunc logOptions $ \logFunc1 -> do
        logOptions' <- setLogUseTime True <$> logOptionsHandle stdout False
        withLogFunc logOptions' $ \logFunc2 -> do
          withPubSocket \sock -> do
            let appState =
                  AppState
                    { _appState_acid = acid
                    , _appState_unbeatenAtsCache = unbeatenAtsCache
                    , _appState_beatenAtsCache = beatenAtsCache
                    , _appState_coreNadeoClient = coreNadeoClient
                    , _appState_liveServicesNadeoClient = liveServicesNadeoClient
                    , _appState_tmxClient = tmxClient
                    , _appState_xertrovClient = xertrovClient
                    , _appState_openPlanetClient = openPlanetClient
                    , _appState_nadeoToken = tokenStateRef
                    , _appState_settings = settings
                    , _appState_jwtSettings = jwtSettings
                    , _appState_nadeoThrottler = throttler
                    , _appState_nadeoRequestRate = 0.5
                    , _appState_logFunc = logFunc1 <> logFunc2
                    , _appState_pubSocket = sock
                    , _appState_checkMapFileQueue = checkMapFileQueue
                    , _appState_s3_conn = conn
                    , _appState_s3_bucket = _s3_creds_bucket $ _settings_s3_creds settings
                    , _appState_syncVars = AppSyncVars
                      { _appSyncVars_validationSem = validationSem
                      }
                    }
            runReaderT m appState

  case _settings_logFile settings of
    Nothing -> go stderr
    Just logFile -> do
      withFile logFile AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        go h

runTemporary :: ( MonadUnliftIO m) =>AcidState MapMonitorState -> ReaderT AppState m b -> m b
runTemporary acid m = do
  x <- newTQueueIO
  runInApp acid x m

runRemotely :: (MonadUnliftIO m) => PortNumber -> ReaderT AppState m c -> m c
runRemotely port m = do
  bracket
    (liftIO $ openRemoteState @MapMonitorState skipAuthenticationPerform "localhost" port)
    (liftIO . closeAcidState)
    (\acid -> runTemporary acid m)

runLocally :: (MonadUnliftIO m) => ReaderT AppState m c -> m c
runLocally m = do
  bracket
    (liftIO $ openLocalState (MapMonitorState mempty))
    (liftIO . closeAcidState)
    (\acid -> runTemporary acid m)
