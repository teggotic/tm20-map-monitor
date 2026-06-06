{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import GHC.Stack
import qualified Data.ByteString as BS
import Protolude hiding (threadDelay, atomically, forkIO)
import qualified RIO.Text as T
import qualified Prelude

import Control.Lens
import MapMonitor.Common

import Network.HTTP.Client as NHC (ManagerSettings (managerModifyRequest), Request (requestHeaders), newManager)
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
import Servant.API
import Servant.Client
import MapMonitor.API (ManagementAPI, GridAPI, AUser (..), AuthAPI, InternalAuth(..))
import Servant.Auth.Server
import RIO.Time (getCurrentTime, addUTCTime)
import MapMonitor.Server
import Data.UUID.V4 (nextRandom)
import System.Directory (doesFileExist)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.UUID (toString)
import Network.HTTP.Client (ManagerSettings(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)
import Control.Category (id)
import MapMonitor.GridDB
import System.Random
import Servant.Auth.Client
import UnliftIO.Temporary (withSystemTempDirectory)
import UnliftIO (tryAny)
import Network.HTTP.Req
import Options.Applicative.Builder (header)
import Network.HTTP.Req.Conduit (responseBodySource)
import System.Process.Typed (proc, runProcess_)

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

(getActiveGrids' :<|> getExpiredGrids' :<|> getGrid' :<|> getGridWithMaps') :<|> createAuthClient = client (Proxy @GridAPI)
_ :<|> _ :<|> fakeAuth' = client (Proxy @AuthAPI)



-- gridId = "aWUS9k1lqv"

runFakeClient :: (HasCallStack, MonadUnliftIO m) => Text -> ClientEnv -> m ()
runFakeClient gId c = do
  accountId <- fmap (T.pack . toString) $ liftIO $ nextRandom
  auth <- fmap (fromRight undefined) $ liftIO $ runClientM (fakeAuth' accountId) c
  let (gridSendMessage' :<|> gridPingConnected' :<|> gridPingDisconnected' :<|> _) = createAuthClient (Token $ encodeUtf8 $ _ia_token auth)
  forever do
    mapConcurrently_ id
        [ forever do
            void $ (liftIO $ runClientM (getActiveGrids') c)
            threadDelay (10 * 10 ^ 6)
        , let loop ts = do
                threadDelay (1 * 10 ^ 6)
                gridME <- liftIO $ runClientM (getGrid' (ID gId) ts) c
                case gridME of
                    Left err -> print err
                    Right Nothing -> loop ts
                    Right (Just grid) -> loop (Just $ _bb_updatedAt grid)
           in forever do loop Nothing
        , do
            grid <- fmap (fromMaybe undefined . fromRight undefined) $ liftIO $ runClientM (getGrid' (ID gId) Nothing) c
            forever do
              x <- randomRIO (5,60)
              isPlaying <- randomIO 
              midx <- randomRIO (0,63)
              forM_ [(1 ::Int)..x] \_ -> do
                void $ liftIO $ runClientM (gridPingConnected' (ID gId) (if isPlaying then Just (_bb_mapUids grid Prelude.!! midx) else Nothing)) c
                threadDelay (5 * 10 ^ 6)
              void $ liftIO $ runClientM (gridPingDisconnected' (ID gId)) c
              threadDelay (5 * 10 ^ 6)
        ]

spawnFakeClients :: (HasCallStack, MonadUnliftIO m) => Text -> Int -> m ()
spawnFakeClients gId n = do
  manager' <-
    liftIO $
      NHC.newManager
        tlsManagerSettings
          { managerModifyRequest = \req -> do
              return req{requestHeaders = requestHeaders req <> [(hUserAgent, "teggot@proton.me; unbeaten-maps-monitor project")]}
          }
  -- let mockClient = (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  let mockClient = (mkClientEnv manager' (BaseUrl Servant.Client.Http "91.98.121.255" 7442 ""))
  mapConcurrently_ (\i -> do putText ("spwaned " <> show i) >> runFakeClient gId mockClient) [1..n]

main :: IO ()
main = do
  getArgs >>= \case
    ["cache-thumbnails"] -> do
      runRemotely 9099 $ do
        conn <- view s3ConnL
        buck <- view s3BucketL
        maps <- filterMaps ((@= Unbeaten) . (@= HiddenOnTmx False))
        flip (pooledMapConcurrentlyN_ 40) (zip [1 :: Int ..] maps) \(i, TMMap {_tmm_tmxId = TMXId mapId}) -> do
          withSystemTempDirectory "map-monitor-download" \dir -> void $ tryAny do
            statE <- liftIO $ runMinioWith conn do
              statObject buck ("thumbnails/" <> show mapId <> ".jpg") defaultGetObjectOptions
            case statE of
              Right _ -> pass
              Left err -> do
                print err
                runReq defaultHttpConfig do
                  reqBr Network.HTTP.Req.GET (https "trackmania.exchange" Network.HTTP.Req./: "mapthumb" /~ mapId) NoReqBody (Network.HTTP.Req.header "User-Agent" "teggot@proton.me; unbeaten-maps-monitor project") $ \r -> do
                    runConduitRes $ (responseBodySource r) .| sinkFileBS (dir </> show mapId)
                runProcess_ (proc "vipsthumbnail" [(dir </> show mapId), "--size", "300x300", "--output", dir </> "out.jpg" <> "[Q=90]"])
                void $ liftIO $ runMinioWith conn $ do
                  fPutObject buck ("thumbnails/" <> show mapId <> ".jpg") (dir </> "out.jpg") defaultPutObjectOptions
                putText $ "Cached thumbnail for " <> show mapId
    ["stress-test", gId, n] -> do
      spawnFakeClients (T.pack gId) $ Prelude.read n
    ["refresh-file-sizes"] -> do
      runRemotely 9099 $ do
        mps <- queryAcid GetMaps
        pooledForConcurrentlyN_ 20 (zip [0::Int ..] mps) \(i, mp) -> do
          updateMapSize mp
          logInfo $ ("updated " <> displayShow i <> "/" <> displayShow (length mps))
    ["recheck-set-with-plugin"] -> do
      runRemotely 9099 $ do
        maps <- fmap (filter $ isNothing . _tmm_atSetByPlugin) $ filterMaps ((@= Unbeaten) . (@= HiddenOnTmx False))
        x <- flip (pooledMapConcurrentlyN 40) (zip [1 :: Int ..] maps) \(i, tmmap) -> do
          atSetByPlugin <- checkAtSetByPlugin tmmap
          logInfo $ "Checking AT set by plugin for map " <> displayShow i <> "/" <> displayShow (length maps) <> " #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> ": " <> displayShow atSetByPlugin
          Protolude.void $ withAcid1 updateMaps $ [TMMapPatch (_tmm_tmxId tmmap) [TMPAtSetByPlugin atSetByPlugin]]
          -- if atSetByPlugin == _tmm_atSetByPlugin tmmap
          --   then return Nothing
          --   else do
          --     logInfo $ "Found a different AT set by plugin for map " <> displayShow i <> "/" <> displayShow (length maps) <> " #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> ": " <> displayShow atSetByPlugin
          --     return $ Just (unTMXId $ _tmm_tmxId tmmap, atSetByPlugin)
        return x
        return ()
        -- Protolude.void $ withAcid1 updateMaps $ [(defPatch $ _tmm_tmxId tmmap){_tmmp_atSetByPlugin = Just atSetByPlugin}]
        -- let mp = Map.fromList $ catMaybes x
        -- print mp
        -- liftIO $ encodeFile "/tmp/at-set-by-plugin1.json" $ mp
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
