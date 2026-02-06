module Main (main) where

import Control.Lens
import Lib
import Data.Acid
import Data.Acid.Remote (acidServer, skipAuthenticationCheck)
import Data.Default
import MapMonitor.Common
import Data.Either.Combinators
import MapMonitor.DB
import MapMonitor.Integrations
import MapMonitor.Server
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip (GzipFiles (GzipCompress), gzip, gzipFiles)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P
import Protolude hiding (withFile, atomically, bracket, forkIO, threadDelay, to, toList, try)
import RIO (MonadUnliftIO, logError, logInfo, displayShow)
import Servant.Auth.Server
import Servant.Server
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Exception (tryAny)
import UnliftIO.STM

import Options.Applicative
import Control.Monad.Logger (runStdoutLoggingT)
import PingRPC

data Options
  = Options
  { opt_runScan :: !Bool
  }
  deriving (Show)

optsP :: Parser Options
optsP = Options <$> flag True False (long "no-scan" <> help "Disable map monitoring")

main :: (MonadUnliftIO m, MonadFail m) => m ()
main = do
  opts <- liftIO $ execParser $ info (optsP <**> helper) mempty

  acid <- liftIO $ openLocalState (MapMonitorState mempty mempty)
  -- liftIO $ createCheckpoint acid
  -- liftIO $ createArchive acid
  unbeatenAtsCache <- flip runReaderT acid $ do
    collectUnbeatenAtsResponse >>= liftIO . newTVarIO

  beatenAtsCache <- flip runReaderT acid $ do
    collectBeatenAtsResponse >>= liftIO . newTVarIO

  runInApp unbeatenAtsCache beatenAtsCache acid $ do
    -- _ <- forkIO $ forever do
    --   processAtCheckQueue checkAtQueue
    _ <- forkIO $ do
      liftIO $ acidServer skipAuthenticationCheck 8082 acid

    when (opt_runScan opts) do
      _ <- forkIO do
        forM_ [(0 :: Int), 20 ..] $ \i -> do
          res <- tryAny $ do
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
            logError $ "Exception happened: " <> displayShow err
          logInfo $ "Refresh happened; sleeping"

          refreshCaches
          threadDelay (20 * 60 * 1000 * 1000)
      pass

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
