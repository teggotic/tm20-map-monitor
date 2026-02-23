{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude hiding (atomically, forkIO)
import qualified RIO.Text as T

import Control.Lens
import Data.Default.Class
import UnliftIO.Concurrent (forkIO, threadDelay)
import MapMonitor.Common
import Network.HTTP.Req
import Network.HTTP.Req.Conduit

import qualified Data.Conduit.Binary as CB
import UnliftIO.Exception (tryAny)
import UnliftIO.STM
import Network.Minio
import Conduit
import System.Environment
import Network.HTTP.Client.TLS
import Network.HTTP.Client as NHC
import UnliftIO.Concurrent
import UnliftIO.Async
import System.FilePath.Posix
import RIO.Text (pack, unpack)
import Lib
import MapMonitor.Integrations
import MapMonitor.Server
import Data.Acid
import MapMonitor.DB
import Data.Conduit.TQueue (sinkTQueue, sinkTBQueue)
import RIO (logSticky, logStickyDone, displayShow, logError)
import MapMonitor.MapCache
import RIO.Prelude (error)

downloadTMXMaps startId = do
  q <- newTBQueueIO 20
  runLocally $ do
    st <- ask
    forM_ [(0::Int)..4] $ \_ -> do
      void $ forkIO $ forever do
        tmmap@(TMMap {_tmm_tmxId = TMXId tmxId}) <- atomically $ readTBQueue q
        void $ flip runReaderT st $ do
          logSticky $ "Downloading map: " <> displayShow tmxId
        void $ flip runReaderT st $ do
          res <- downloadTmxMapToS3 tmmap
          case res of
            Left err -> do
              logStickyDone $ "Exception happened: " <> displayShow err
            Right True -> error "Found a map that was already in the cache"
            Right False -> pass

    runConduit $
      tmxMapsSource 200 startId
      .| sinkTBQueue q

main :: IO ()
main = do
  getArgs >>= \case
    ["download-all-tmx", readMaybe -> startId] -> do
      downloadTMXMaps startId
    ["upload-map", mapFile] -> do
      runLocally $ do
        conn <- view s3ConnL
        res <- liftIO $ runMinioWith conn $ do
          fPutObject "map-monitor-test" (T.pack $ takeFileName mapFile) mapFile defaultPutObjectOptions
        print res
    _ -> pass
