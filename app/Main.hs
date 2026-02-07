module Main (main) where

import Lib
import Control.Monad.Logger
import Data.Acid
import Data.Acid.Remote (acidServer, openRemoteState, skipAuthenticationCheck, skipAuthenticationPerform)
import Data.Default
import Data.Either.Combinators
import Dhall
import MapMonitor.CachedAPIResponses
import MapMonitor.DB
import MapMonitor.Common
import MapMonitor.Integrations
import MapMonitor.Server
import MapMonitor.Common
import Network.HTTP.Client as NHC (ManagerSettings (managerModifyRequest), Request (requestHeaders), newManager)
import Network.HTTP.Client.TLS
import PingRPC
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

main :: (MonadUnliftIO m, MonadFail m) => m ()
main = do
  acid <- liftIO $ openLocalState (MapMonitorState mempty mempty)
  -- liftIO $ createCheckpoint acid
  -- liftIO $ createArchive acid
  unbeatenAtsCache <- flip runReaderT acid $ do
    collectUnbeatenAtsResponse >>= liftIO . newTVarIO

  beatenAtsCache <- flip runReaderT acid $ do
    collectBeatenAtsResponse >>= liftIO . newTVarIO

  -- checkAtQueue <- newTQueueIO
  --
  runInApp unbeatenAtsCache beatenAtsCache acid $ do
    -- _ <- forkIO $ forever do
    --   processAtCheckQueue checkAtQueue
    _ <- forkIO $ do
      liftIO $ acidServer skipAuthenticationCheck 8082 acid

    _ <- forkIO $ do
      runStdoutLoggingT runClient

    _ <- forkIO do
      forever do
        res <- tryAny do
          refreshCaches
        whenLeft res $ \err ->
          putText $ "Exception happened: " <> show err

        threadDelay (24 * 60 * 60 * 1000000)

    _ <- forkIO do
      forM_ [(0 :: Int), 20 ..] $ \i -> do
        res <- tryAny $ do
          pass
          refreshMissingInfo

          when (i /= 0) do
            if i `mod` 180 == 0
              then refreshUnbeatenMaps
              else
                if i `mod` 60 == 0
                  then refreshRecentUnbeatenMaps
                  else pass
          if i `mod` 24 * 60 == 0
             then Protolude.void $ recheckTmxForLatestMissingMaps 1000
             else Protolude.void $ recheckTmxForLatestMissingMaps 80

        whenLeft res $ \err ->
          putText $ "Exception happened: " <> show err

        refreshCaches
        threadDelay (20 * 60 * 1000 * 1000)

    st <- ask
    let
      settings =
        setPort 8081 $
          defaultSettings
      cookieCfg = defaultCookieSettings
      cfg = cookieCfg :. (_appState_jwtSettings st) :. EmptyContext

    _ <- P.register P.ghcMetrics

    liftIO $
      runSettings settings $
        gzip (def{gzipFiles = GzipCompress}) $
          P.prometheus P.def $
            app cfg st
