{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Lib
import Data.Acid
import Data.Acid.Remote (acidServer, skipAuthenticationCheck)
import Data.Default.Class
import Data.Either.Combinators
import MapMonitor.DB
import MapMonitor.Integrations
import MapMonitor.Server
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip (GzipFiles (GzipCompress), gzip, gzipFiles)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P
import Protolude hiding (killThread, withFile, atomically, bracket, forkIO, threadDelay, to, toList, try)
import RIO (MonadUnliftIO, logError, logInfo, displayShow)
import Servant.Auth.Server
import Servant.Server
import UnliftIO.Concurrent (forkIO, threadDelay, killThread)
import UnliftIO.Exception (tryAny)
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
  (_, acid) <- allocate (liftIO $ openLocalState (MapMonitorState mempty)) (liftIO . closeAcidState)
  -- liftIO $ createCheckpoint acid
  -- liftIO $ createArchive acid
  checkMapFileQueue <- newTQueueIO

  runInApp acid checkMapFileQueue $ do
    void $ flip allocateU killThread $ forkIO $ forever do
      tryAny (processMapFileQueue checkMapFileQueue)
        >>= \case
          Left err -> logError $ "Error processing map file queue: " <> displayShow err
          Right _ -> pass

    void $ flip allocateU killThread $ forkIO $ do
      liftIO $ acidServer skipAuthenticationCheck 8082 acid

    when (opt_runScan opts) do
      void $ flip allocateU killThread $ forkIO $ do
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
      cfg = cookieCfg :. (_appState_jwtSettings st) :. EmptyContext

    _ <- P.register P.ghcMetrics

    liftIO $
      runSettings settings $
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
