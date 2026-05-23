{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude hiding (atomically, forkIO)
import qualified RIO.Text as T

import Control.Lens
import MapMonitor.Common

import Conduit
import Data.Aeson
import Data.Conduit.TQueue (sinkTBQueue)
import Data.IxSet.Typed
import Lib
import MapMonitor.ATCheck
import MapMonitor.DB
import MapMonitor.Integrations
import MapMonitor.MapCache
import MapMonitor.MapMonitorTools (mapHasClones)
import Network.Minio
import RIO (displayShow, logInfo, logSticky, logStickyDone)
import qualified RIO.Map as Map
import RIO.Prelude (error)
import System.FilePath.Posix
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.STM

downloadTMXMaps startId = do
  q <- newTBQueueIO 20
  runRemotely 9099 $ do
    st <- ask
    forM_ [(0 :: Int) .. 4] $ \_ -> do
      void $ forkIO $ forever do
        tmmap@(TMMap{_tmm_tmxId = TMXId tmxId}) <- atomically $ readTBQueue q
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
    ["refresh-file-sizes"] -> do
      runRemotely 9099 $ do
        mps <- queryAcid GetMaps
        pooledForConcurrentlyN_ 20 (zip [0 ..] mps) \(i, mp) -> do
          updateMapSize mp
          logInfo $ ("updated " <> displayShow i <> "/" <> displayShow (length mps))
    ["recheck-set-with-plugin"] -> do
      runRemotely 9099 $ do
        maps <- filterMaps ((@= Unbeaten) . (@= HiddenOnTmx False))
        x <- flip (pooledMapConcurrentlyN 40) (zip [1 :: Int ..] maps) \(i, tmmap) -> do
          atSetByPlugin <- checkAtSetByPlugin tmmap
          logInfo $ "Checking AT set by plugin for map " <> displayShow i <> "/" <> displayShow (length maps) <> " #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> ": " <> displayShow atSetByPlugin
          if atSetByPlugin == _tmm_atSetByPlugin tmmap
            then return Nothing
            else do
              logInfo $ "Found a different AT set by plugin for map " <> displayShow i <> "/" <> displayShow (length maps) <> " #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> ": " <> displayShow atSetByPlugin
              return $ Just (unTMXId $ _tmm_tmxId tmmap, atSetByPlugin)
        -- Protolude.void $ withAcid1 updateMaps $ [(defPatch $ _tmm_tmxId tmmap){_tmmp_atSetByPlugin = Just atSetByPlugin}]
        let mp = Map.fromList $ catMaybes x
        print mp
        liftIO $ encodeFile "/tmp/at-set-by-plugin1.json" $ mp
    ["recheck-has-clones"] -> do
      runRemotely 9099 $ do
        maps <- filter (isNothing . _tmm_hasClones) <$> filterMaps ((@= Unbeaten) . (@= HiddenOnTmx False))
        print $ length maps
        flip (pooledMapConcurrentlyN_ 10) (zip [1 :: Int ..] maps) \(i, tmmap) -> do
          mapHasClones tmmap
            >>= \case
              Just x -> do
                Protolude.void $ withAcid1 updateMaps $ [TMMapPatch (_tmm_tmxId tmmap) [TMPClones $ Just x]]
              _ -> return ()
    -- case atSetByPlugin of
    --   Just True -> do
    --     logInfo $ "Found a map with clones by plugin for map " <> displayShow i <> "/" <> displayShow (length maps) <> " #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> ": " <> displayShow atSetByPlugin
    --     return $ Just tmmap
    --   _ -> return Nothing
    -- logInfo $ "Checking has clones by plugin for map " <> displayShow i <> "/" <> displayShow (length maps) <> " #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> ": " <> displayShow atSetByPlugin
    -- print $ length $ catMaybes x
    -- mapM_ print $ catMaybes x
    ["download-all-tmx", readMaybe -> startId] -> do
      downloadTMXMaps startId
    ["upload-notes", mapFile] -> do
      runRemotely 9099 $ do
        conn <- view s3ConnL
        res <- liftIO $ runMinioWith conn $ do
          fPutObject "tm20" (T.pack $ "notes" </> takeFileName mapFile) mapFile defaultPutObjectOptions
        print res
    ["upload-map", mapFile] -> do
      runRemotely 9099 $ do
        conn <- view s3ConnL
        res <- liftIO $ runMinioWith conn $ do
          fPutObject "map-monitor-test" (T.pack $ takeFileName mapFile) mapFile defaultPutObjectOptions
        print res
    _ -> pass
