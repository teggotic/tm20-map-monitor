{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import Data.Acid
import Data.Acid.Remote (acidServer, skipAuthenticationCheck)
import Data.Default.Class
import Data.Either.Combinators
import Lib
import MapMonitor.DB
import MapMonitor.Integrations
import MapMonitor.Server
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip (GzipFiles (GzipCompress), gzip, gzipFiles)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P
import Protolude hiding (atomically, bracket, forkIO, killThread, threadDelay, to, toList, try, withFile)
import RIO (MonadUnliftIO, displayShow, logError, logInfo)
import Servant.Auth.Server
import Servant.Server
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM

import Options.Applicative
import UnliftIO.Exception
import UnliftIO.Resource

data Options
  = Options
  { opt_runScan :: !Bool
  }
  deriving (Show)

optsP :: Parser Options
optsP = Options <$> flag True False (long "no-scan" <> help "Disable map monitoring")

runMain :: (MonadUnliftIO m, MonadFail m) => Options -> m ()
runMain opts = runResourceT $ do
  putText "Starting up"
  (_, acid) <- allocate (liftIO $ openLocalState (MapMonitorState mempty mempty)) (liftIO . closeAcidState)
  putText "Opened acid state"
  checkMapFileQueue <- newTQueueIO

  runInApp acid checkMapFileQueue $ do
    logInfo "Loaded environment"
    spawnThread $ forever do
      tryAny (processMapFileQueue checkMapFileQueue)
        >>= \case
          Left err -> logError $ "Error processing map file queue: " <> displayShow err
          Right _ -> pass
      refreshCaches

    spawnThread do
      liftIO $ acidServer skipAuthenticationCheck 8082 acid

    when (opt_runScan opts) do
      spawnThread do
        forM_ [(0 :: Int), 20 ..] $ \i -> do
          res <- tryAny $ do
            refreshMissingInfo

            when (i `mod` 600 == 0) do
              recheckTmxInfo
              recheckMapsUnhidden

            when (i /= 0) do
              if i `mod` 180 == 0
                then refreshUnbeatenMaps
                else
                  if i `mod` 60 == 0
                    then refreshRecentUnbeatenMaps
                    else pass
            if i `mod` 24 * 60 == 0
              then Protolude.void $ scanTmx (Just 1000)
              else Protolude.void $ scanTmx (Just 80)

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
      cfg = cookieCfg :. (_appState_jwtSettings st) :. (_appState_responseCache st) :. EmptyContext

    _ <- P.register P.ghcMetrics

    logInfo "Starting API"
    liftIO $
      runSettings settings $
        simpleCors $
          gzip (def{gzipFiles = GzipCompress}) $
            P.prometheus P.def $
              app cfg st

runDev :: IO ()
runDev = do
  runMain $ Options False

main :: (MonadUnliftIO m, MonadFail m) => m ()
main = do
  opts <- liftIO $ execParser $ info (optsP <**> helper) mempty
  runMain opts
