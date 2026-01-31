{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Data.Acid
import Data.Acid.Remote (acidServer, openRemoteState, skipAuthenticationCheck, skipAuthenticationPerform)
import Data.Default
import Data.Either.Combinators
import Dhall
import MapMonitor.CachedAPIResponses
import MapMonitor.DB
import MapMonitor.Integrations
import MapMonitor.Server
import Network.HTTP.Client as NHC (ManagerSettings (managerModifyRequest), Request (requestHeaders), newManager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (hUserAgent)
import Network.Socket (PortNumber)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip (GzipFiles (GzipCompress), gzip, gzipFiles)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P
import Protolude hiding (withFile, atomically, bracket, forkIO, threadDelay, to, toList, try)
import RIO (MonadUnliftIO, newTMVarIO, withLogFunc, setLogUseTime, logOptionsHandle, withFile, hSetBuffering, BufferMode (LineBuffering))
import Servant.Auth.Server
import Servant.Client
import Servant.Server
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Exception (bracket, tryAny)
import UnliftIO.STM
import System.Directory (doesFileExist)
import RIO.Time (getCurrentTime)

runInApp :: (MonadUnliftIO m) => TVar UnbeatenAtsResponse -> TVar RecentlyBeatenAtsResponse -> AcidState MapMonitorState -> ReaderT AppState m b -> m b
runInApp unbeatenAtsCache beatenAtsCache acid m = do
  settings <- liftIO $ input auto "./settings.dhall"
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

  throttler <- newTMVarIO =<< getCurrentTime

  tokenStateRef <- RIO.newTMVarIO Nothing
  -- withFile "/tmp/tm20-map-monitor.log" AppendMode $ \h -> do
  let h = stderr
  do
    hSetBuffering h LineBuffering
    logOptions' <- logOptionsHandle h False
    let logOptions = setLogUseTime True logOptions'
    withLogFunc logOptions $ \logFunc -> do
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
              , _appState_nadeoRequestRate = 1
              , _appState_logFunc = logFunc
              }
      runReaderT m appState

runTemporary :: ( MonadUnliftIO m) =>AcidState MapMonitorState -> ReaderT AppState m b -> m b
runTemporary acid m = do
  unbeatenAtsCache <- flip runReaderT acid $ do
    collectUnbeatenAtsResponse >>= liftIO . newTVarIO
  beatenAtsCache <- flip runReaderT acid $ do
    collectBeatenAtsResponse >>= liftIO . newTVarIO
  runInApp unbeatenAtsCache beatenAtsCache acid m

runRemotely :: (MonadUnliftIO m) => PortNumber -> ReaderT AppState m c -> m c
runRemotely port m = do
  bracket
    (liftIO $ openRemoteState @MapMonitorState skipAuthenticationPerform "localhost" port)
    (liftIO . closeAcidState)
    (\acid -> runTemporary acid m)

runLocally :: (MonadUnliftIO m) => ReaderT AppState m c -> m c
runLocally m = do
  bracket
    (liftIO $ openLocalState (MapMonitorState mempty mempty))
    (liftIO . closeAcidState)
    (\acid -> runTemporary acid m)

runProd :: (MonadUnliftIO m, MonadFail m) => m ()
runProd = do
  acid <- liftIO $ openLocalState (MapMonitorState mempty mempty)
  -- liftIO $ createCheckpoint acid
  -- liftIO $ createArchive acid
  unbeatenAtsCache <- flip runReaderT acid $ do
    collectUnbeatenAtsResponse >>= liftIO . newTVarIO

  beatenAtsCache <- flip runReaderT acid $ do
    collectBeatenAtsResponse >>= liftIO . newTVarIO

  checkAtQueue <- newTQueueIO

  runInApp unbeatenAtsCache beatenAtsCache acid $ do
    _ <- forkIO $ forever do
      processAtCheckQueue checkAtQueue

    fullRescan checkAtQueue

    -- _ <- forkIO $ do
    --   liftIO $ acidServer skipAuthenticationCheck 8082 acid
    --

    

    -- _ <- forkIO do
    --   forever do
    --     threadDelay (24 * 60 * 60 * 1000000)
    --
    --     res <- tryAny do
    --       recheckTmxForLatestMissingMaps 1000
    --       refreshCaches
    --     whenLeft res $ \err ->
    --       putText $ "Exception happened: " <> show err
    --
    -- _ <- forkIO do
    --   forM_ [(0 :: Int), 20 ..] $ \i -> do
    --     res <- tryAny $ do
    --       pass
    --       refreshMissingInfo
    --
    --       when (i /= 0) do
    --         if i `mod` 180 == 0
    --           then refreshUnbeatenMaps
    --           else
    --             if i `mod` 60 == 0
    --               then refreshRecentUnbeatenMaps
    --               else pass
    --       recheckTmxForLatestMissingMaps 80
    --
    --     whenLeft res $ \err ->
    --       putText $ "Exception happened: " <> show err
    --
    --     refreshCaches
    --     threadDelay (20 * 60 * 1000 * 1000)
    --
    -- st <- ask
    -- let
    --   settings =
    --     setPort 8081 $
    --       defaultSettings
    --   cookieCfg = defaultCookieSettings
    --   cfg = cookieCfg :. (_appState_jwtSettings st) :. EmptyContext
    --
    -- _ <- P.register P.ghcMetrics
    --
    -- liftIO $
    --   runSettings settings $
    --     gzip (def{gzipFiles = GzipCompress}) $
    --       P.prometheus P.def $
    --         app cfg st
