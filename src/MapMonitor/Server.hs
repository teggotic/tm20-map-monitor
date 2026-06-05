{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module MapMonitor.Server
where

import Control.Category (id)
import Control.Exception (throw)
import Control.Lens hiding ((.=), (<.>))
import Control.Retry (limitRetries)
import Data.Acid
import Data.Cache as Cache
import Data.Fixed
import Data.IxSet.Typed
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import MapMonitor.API
import MapMonitor.API.Nadeo
import MapMonitor.API.OpenPlanet
import MapMonitor.API.TMX
import MapMonitor.API.Util
import MapMonitor.API.XertroV
import MapMonitor.Common
import MapMonitor.DB
import MapMonitor.Integrations
import MapMonitor.ReplayValidation
import MapMonitor.ServantCache (ResponseCache (..))
import Network.HTTP.Req
import qualified Network.HTTP.Types as H
import Network.Minio
import Network.Wai as Wai
import PingRPC
import Protolude hiding (forkIO, atomically, finally, threadDelay, wait, withAsync, (<.>))
import RIO (HasLogFunc (..), LogFunc, displayShow, finally, logError, logInfo, toStrictBytes)
import qualified RIO.Text as T
import qualified RIO.Text as Text
import RIO.Time
import Servant
import Servant.Auth.Server
import Servant.Client hiding ((/:))
import Servant.Server
import qualified System.ZMQ4 as ZMQ
import UnliftIO.Async
import UnliftIO.Directory (removeFile)
import UnliftIO.Exception (tryAny)
import UnliftIO.Concurrent
import UnliftIO.STM
import Conduit
import System.Clock (getTime, Clock (Monotonic), TimeSpec (TimeSpec))
import MapMonitor.GridDB (BBTable, GetBoard (..), BBTableUpdate (..), Grid (..), ID (..), UpdateBBTable (..), PosixTS (..), GetBoardUpdatedAfter (..), ChatMessage (..), GetBoardsByIds (..), GetBoardsByUser (..))
import UnliftIO.Temporary
import Network.HTTP.Req.Conduit (responseBodySource)
import RIO.FilePath ((</>))
import System.Process.Typed (proc, runProcess_)
import Data.Acid.Abstract
import Test.RandomStrings
import qualified StmContainers.Map as STM
import qualified ListT
import MapMonitor.MapCache (s3MapPath)

type GridPlayerDB = Cache Text (Maybe TrackUid)

data AppState
  = AppState
  { _appState_acid :: !(AcidState MapMonitorState)
  , _appState_coreNadeoClient :: !ClientEnv
  , _appState_liveServicesNadeoClient :: !ClientEnv
  , _appState_trackmaniaComClient :: !ClientEnv
  , _appState_tmxClient :: !ClientEnv
  , _appState_xertrovClient :: !ClientEnv
  , _appState_openPlanetClient :: !ClientEnv
  , _appState_nadeoToken :: !(TMVar (Maybe NadeoTokenState))
  , _appState_settings :: !AppSettings
  , _appState_jwtSettings :: !JWTSettings
  , _appState_nadeoThrottler :: !(TMVar UTCTime)
  , _appState_nadeoRequestRate :: !Pico
  , _appState_logFunc :: !LogFunc
  , _appState_pubSocket :: !(ZMQ.Socket ZMQ.Pub)
  , _appState_checkMapFileQueue :: !(TQueue TMMap)
  , _appState_s3_conn :: !MinioConn
  , _appState_s3_bucket :: !Text
  , _appState_syncVars :: !AppSyncVars
  , _appState_displayNamesCache :: !(Cache Text Text)
  , _appState_responseCache :: !ResponseCache
  , _appState_notifyCache :: !(Cache Text ())
  , _appState_gridDB :: (AcidState BBTable)
  , _appState_thumbnailCache :: !(Cache Int ())
  , _appState_gridPlayers :: (STM.Map ID GridPlayerDB)
  }

$(makeLenses ''AppState)

instance HasSyncVars AppState where
  syncVarsL = appState_syncVars

instance HasAppSettings AppState where
  appSettingsL = appState_settings

instance HasState AppState where
  stateL = appState_acid

instance HasState (AcidState MapMonitorState) where
  stateL = id

instance HasNadeoCoreClient AppState where
  nadeoCoreClientL = appState_coreNadeoClient

instance HasNadeoAuthToken AppState where
  nadeoAuthTokenL = appSettingsL . settings_auth

instance HasXertrovClient AppState where
  xertrovClientL = appState_xertrovClient

instance HasNadeoLiveClient AppState where
  nadeoLiveClientL = appState_liveServicesNadeoClient

instance HasTMXClient AppState where
  tmxClientL = appState_tmxClient

instance HasOpenPlanetClient AppState where
  openPlanetClientL = appState_openPlanetClient

instance HasNadeoTokenState AppState where
  nadeoTokenStateL = appState_nadeoToken

instance HasNadeoThrottler AppState where
  nadeoThrottlerL = appState_nadeoThrottler

instance HasNadeoRequestRate AppState where
  nadeoRequestRateL = appState_nadeoRequestRate

instance HasLogFunc AppState where
  logFuncL = appState_logFunc

instance HasPubRpcSocket AppState where
  pubRpcSocketL = appState_pubSocket

instance HasCheckMapFileQueue AppState where
  checkMapFileQueueL = appState_checkMapFileQueue

instance HasS3Connection AppState where
  s3ConnL = appState_s3_conn
  s3BucketL = appState_s3_bucket

instance HasResponseCache AppState where
  responseCacheL = appState_responseCache

instance HasNotifyCache AppState where
  notifyCacheL = appState_notifyCache

instance HasGridDB AppState where
  gridDBL = appState_gridDB
  
type AppM = ReaderT AppState Servant.Server.Handler

mapMonitorAPI :: Proxy MapMonitorAPI
mapMonitorAPI = Proxy
redirectTo :: Text -> Wai.Response
redirectTo url =
  responseLBS
    H.status302
    [(H.hContentType, "text/plain"), (H.hLocation, encodeUtf8 url)]
    "Redirect"

downloadMapsServer :: AppState -> ServerT DownloadMapAPI AppM
downloadMapsServer st = downloadMap :<|> mapThumbnail :<|> notifyMapBeaten
 where
  downloadMap :: Int -> ServerT Raw AppM
  downloadMap mapId = do
    Tagged $ \_ resp -> do
      tmmaps <- liftIO $ flip runReaderT st $ do
        putText $ "Testing tmx"
        result <-
          liftIO $
            tryAny $
              runReq (defaultHttpConfig{httpConfigRetryPolicy = limitRetries 0}) $
                req Network.HTTP.Req.GET (https "trackmania.exchange" /: "mapgbx" /~ mapId) NoReqBody ignoreResponse (responseTimeout (2 * 1000000))
        case result of
          Left _ -> do
            putText $ "Tmx doesnot work"
            runInClient tmxClientL (tmxSearchMaps TMXSearchMaps{_tmxsm_ids = [mapId], _tmxsm_count = Just 100, _tmxsm_after = Nothing, _tmxsm_from = Nothing, _tmxsm_order1 = Nothing})
              >>= \case
                Left err -> do
                  logError $ "Error: " <> displayShow err
                  return $ Left []
                Right res -> do
                  logInfo $ "Got " <> displayShow (_tmxsr_Results res)
                  let tmmaps = catMaybes $ tmxMapToTMMap <$> _tmxsr_Results res
                  case tmmaps of
                    [tmmap] -> do
                      nadeoGetMapMultiple (IdsList [_tmm_uid tmmap])
                        >>= \case
                          Left err -> do
                            logError $ "Error: " <> displayShow err
                            flip runReaderT st $ do
                              buck <- view s3BucketL
                              host <- view $ appSettingsL . settings_s3_creds . s3_creds_host
                              queryAcid (GetMapById $ TMXId mapId)
                                >>= \case
                                  Nothing -> return $ Left []
                                  Just (TMMap{_tmm_uid = uid}) -> do
                                    return $ Left $ ["https://" <> buck <> "." <> host <> "/" <> s3MapPath uid]
                          Right res2 -> do
                            logInfo $ "Got " <> displayShow (_gmmr_mapList res2) <> " maps"
                            return $ Left $ _gmmrm_downloadUrl <$> _gmmr_mapList res2
                    _ -> do
                      return $ Left []
          Right _ -> do
            putText $ "TMX works"
            return $ Right mapId
      case tmmaps of
        Left [tmmap] -> do
          putText $ "Redirecting to map " <> tmmap
          resp $ redirectTo $ tmmap
        Right _ -> do
          putText $ "Redirecting to map " <> show mapId
          resp $ redirectTo $ "https://trackmania.exchange/maps/download/" <> show mapId
        _ -> throw err404

  mapThumbnail :: Int -> ServerT Raw AppM
  mapThumbnail mapId = do
    Tagged $ \_ resp -> do
      url <- liftIO $ flip runReaderT st $ do
        conn <- view s3ConnL
        buck <- view s3BucketL
        host <- view $ appSettingsL . settings_s3_creds . s3_creds_host
        statE <- liftIO $ runMinioWith conn do
          statObject buck ("thumbnails/" <> show mapId <> ".jpg") defaultGetObjectOptions
        case statE of
          Right _ -> return $ "https://" <> buck <> "." <> host <> "/thumbnails/" <> show mapId <> ".jpg"
          Left _ -> do
            let cache = _appState_thumbnailCache st
            now <- liftIO $ getTime Monotonic
            shouldUpdateS3 <- atomically $ do
              bM <- lookupSTM False mapId cache now
              case bM of
                Nothing -> do
                  insertSTM mapId () cache (Just $ TimeSpec 60 0)
                  return True
                Just _ -> return False
            when shouldUpdateS3 do
              void $ forkIO $ do
                withSystemTempDirectory "map-monitor-download" \dir -> void $ tryAny do
                  runReq defaultHttpConfig do
                    reqBr Network.HTTP.Req.GET (https "trackmania.exchange" /: "mapthumb" /~ mapId) NoReqBody (header "User-Agent" "teggot@proton.me; unbeaten-maps-monitor project") $ \r -> do
                      runConduitRes $ (responseBodySource r) .| sinkFileBS (dir </> show mapId)
                  runProcess_ (proc "vipsthumbnail" [(dir </> show mapId), "--size", "300x300", "--output", dir </> "out.jpg" <> "[Q=90]"])
                  void $ liftIO $ runMinioWith conn $ do
                    fPutObject buck ("thumbnails/" <> show mapId <> ".jpg") (dir </> "out.jpg") defaultPutObjectOptions
            return $ "https://trackmania.exchange/mapthumb/" <> show mapId
      resp $ redirectTo $ url

  notifyMapBeaten mapUid = do
    runAppState do 
      void $ forkIO $ do
        logInfo $ "Notify request: " <> displayShow mapUid
        now <- liftIO $ getTime Monotonic
        notifyCache <- view $ notifyCacheL
        liftIO $ purgeExpired notifyCache
        whenM (atomically do
          existsM <- lookupSTM True mapUid notifyCache now
          case existsM of
            Just _ -> return False
            Nothing -> do
              insertSTM mapUid () notifyCache (Just $ TimeSpec 5 0)
              return True
          ) do
            forM_ [1 :: Int, 3, 5, 8, 12] \i -> do
              threadDelay $ i * (10 ^ (6 :: Int))
              queryAcid (GetMapByUid mapUid) >>= \case
                Nothing -> pass
                Just tmmap -> do
                  when (isMapUnbeaten tmmap) do
                    runConduit $ Conduit.yield tmmap .| refreshMapRecordC Nothing .| sinkNull
                    refreshCaches
    return NoContent

-- return $ Left tmmaps
-- Right _ -> do
--   return $ Right mapId
-- return tmmaps

tmxApiServer :: ServerT TMXApi AppM
tmxApiServer
     = unbeaten
  :<|> unbeatenV2
  :<|> unbeatenLeaderboard
  :<|> beaten
  :<|> unbeatenCount
  :<|> doPurgeCache
  :<|> mapsBeatenInfo
 where
  beaten = do
    collectBeatenAtsResponse

  unbeatenLeaderboard = do
    return $
      UnbeatenAtsLeaderboardResponse
        { _ualr_count_to_pos = mempty
        , _ualr_players = [("c331bdbf-2182-4a51-813d-87d6f0f209c5", 1)]
        , _ualr_nb_players = 1
        , _ualr__info = "{ count_to_pos: {[score]: (rank, nb_eq_players)}, players: [(user, score)] }"
        }

  unbeaten = do
    collectUnbeatenAtsResponse

  unbeatenV2 = do
    collectUnbeatenAtsResponseV2

  unbeatenCount = do
    maps <-
      filter (\x -> (Protolude.null $ _tmm_info x) && (isNothing $ _tmm_hiddenReason x) && (_tmm_hasClones x /= Just True) && not (_tmm_hiddenOnTmx x))
        <$> filterMaps ((@= HasNadeoInfo True) . (@= (TrackType $ Just MT_Race)) . (@= Unbeaten))
    let
      totalUnbeaten = length maps
      totalNonAltNadeo = length $ filter (\x -> not (49 `elem` _tmm_tags x)) maps
    return $ "Total unbeaten: " <> show totalUnbeaten <> ", not alt nadeo: " <> show totalNonAltNadeo

  doPurgeCache = do
    refreshCaches
    return NoContent

  mapsBeatenInfo mapUids = do
    when (length mapUids > 100) $ throwError err401
    runAppState $ do
      host <- view $ appSettingsL . settings_s3_creds . s3_creds_host
      maps <- filter (not . _tmm_hiddenOnTmx) <$> queryAcid (GetMapsByUid mapUids)
      return $ collectMapInfoResponse host maps

managementApiServer :: AuthResult AUser -> ServerT ManagementAPI AppM
managementApiServer (Authenticated auser) =
       (managementReportMap :<|> managementDeleteReport :<|> managementAddMissingMap)
 where
  managementReportMap tmxId payload = do
    unless (_auser_uid auser `elem` trustedUsers) $ throwError err403
    -- putText $ "Reporting map: " <> show tmxId <> " with payload: " <> show payload
    now <- getCurrentTime
    withAcid2 reportMap (TMXId tmxId) (_auser_uid auser, now, _rmp_reason payload)
    refreshCaches
    return NoContent

  managementDeleteReport tmxId = do
    unless (_auser_uid auser `elem` trustedUsers) $ throwError err403
    void $ withAcid1 updateMaps $ [TMMapPatch (TMXId tmxId) [TMPReportedBy $ Map.fromList [(_auser_uid auser, Nothing)]]]
    refreshCaches
    return NoContent

  managementAddMissingMap tmxId = do
    unless (_auser_uid auser `elem` trustedUsers) $ throwError err403
    runAppState do
      addMissingMaps [tmxId]
    refreshCaches
    return NoContent
managementApiServer _ = do
  throwAll err401

getGridPlayers :: (MonadIO m) => STM.Map ID GridPlayerDB -> ID -> m GridPlayerDB
getGridPlayers mp k = atomically $ do
  STM.lookup k mp >>= \case
    Nothing -> do
      c <- newCacheSTM $ Just (TimeSpec 60 0)
      STM.insert c k mp
      return c
    Just c -> return c

gridApiServer :: ServerT GridAPI AppM
gridApiServer = (getActiveGrids :<|> getExpiredGridsBy :<|> getGrid :<|> getPlayers :<|> getGridWithMapInfo) :<|> authGrids
 where
  getGrid gId updatedAfterM = do
    runAppState $ do
      bbAcid <- view gridDBL
      case updatedAfterM of
        Nothing -> query' bbAcid $ GetBoard gId
        Just after -> query' bbAcid $ GetBoardUpdatedAfter gId after

  getGridWithMapInfo gId = do
    runAppState $ do
      bbAcid <- view gridDBL
      host <- view $ appSettingsL . settings_s3_creds . s3_creds_host
      gridM <- query' bbAcid $ GetBoard gId
      case gridM of
        Nothing -> return Nothing
        Just grid -> do
          maps <- queryAcid $ GetMapsByUid (_bb_mapUids grid)
          return $ Just $
            GridWithMapsResponse
            { _gwmr_grid = grid
            , _gwmr_maps = collectMapInfoResponse host $ filter (not . _tmm_hiddenOnTmx) maps
            }

  getActiveGrids = do
    runAppState $ do
      bbAcid <- view gridDBL
      players <- view appState_gridPlayers
      activeIds <- fmap catMaybes $ liftIO $ ListT.toList $ do
        (gId, c) <- STM.listTNonAtomic players
        liftIO $ purgeExpired c
        atomically do
          sz <- sizeSTM c
          if sz > 0
            then return $ Just (gId, sz)
            else do
              STM.delete gId players
              return Nothing

      let mp = Map.fromList activeIds
      fmap (fmap (\grid -> EnrichedGrid grid $ fromMaybe 0 $ Map.lookup (_bb_uuid grid) mp)) $ query' bbAcid $ GetBoardsByIds $ fmap fst activeIds

  getPlayers gId = do
    runAppState $ do
      players <- view appState_gridPlayers
      cache <- getGridPlayers players gId
      -- let x :: Int
      --     x = cache
      xs <- liftIO $ Cache.toList cache
      return $ fmap (\(k,v,_) -> (k, v)) xs 

  getExpiredGridsBy userUid = do
    runAppState $ do
      bbAcid <- view gridDBL
      query' bbAcid $ GetBoardsByUser userUid 10

  authGrids (Authenticated auser) = (postMessage :<|> pingConnected :<|> pingDisconnected :<|> replaceMaps :<|> publishGrid)
    where
      postMessage gId message = do
        when (Text.null message) $ throwError err400
        bbAcid <- runAppState $ view gridDBL
        now <- getCurrentTime
        query' bbAcid (GetBoard gId)
          >>= \case Nothing -> throwError err404
                    Just _ -> update' bbAcid $ UpdateBBTable now $ BBTUAddChatMessage gId (ChatMessage 0 message (_auser_uid auser) (PosixTS now))
        return NoContent

      publishGrid pgBody = do
        when (Text.null $ _pg_name pgBody) $ throwError err400
        runAppState $ do
          bbAcid <- view gridDBL
          now <- getCurrentTime
          let
              genNewCode 0 = throw err500
              genNewCode n = do
                boardCode <- fmap (ID . T.pack) $ liftIO $ randomString (onlyAlphaNum randomASCII) 10
                (query' bbAcid $ GetBoard boardCode) >>= \case
                  Nothing -> return boardCode
                  Just _ -> genNewCode ((n :: Int) - 1)
          boardCode <- genNewCode 10
          let grid = Grid
                { _bb_uuid = boardCode
                , _bb_name = _pg_name pgBody
                , _bb_mapUids = _pg_mapIds pgBody
                , _bb_size = _pg_size pgBody
                , _bb_chatLog = mempty
                , _bb_authorUid = (_auser_uid auser)
                , _bb_createdAt = PosixTS now
                , _bb_updatedAt = PosixTS now
                }

          players <- view appState_gridPlayers
          cache <- getGridPlayers players boardCode
          liftIO $ Cache.insert cache (_auser_uid auser) Nothing

          update' bbAcid $ UpdateBBTable now $ BBTUAddBoard grid
          return grid

      pingConnected gId mapUidM = do
        -- putText $ "pinged " <> show gId <> " " <> show mapUidM
        runAppState $ do
          players <- view appState_gridPlayers
          cache <- getGridPlayers players gId
          liftIO $ Cache.insert cache (_auser_uid auser) mapUidM
          -- void $ liftIO $ scheduleUpdate bbAcid $ UpdateBBTable now $ BBTUPingConnected gId (_auser_uid auser) mapUidM
          pass
        return NoContent

      pingDisconnected gId = do
        runAppState $ do
          players <- view appState_gridPlayers
          cache <- getGridPlayers players gId
          liftIO $ Cache.delete cache (_auser_uid auser)
          pass
        return NoContent

      replaceMaps gId mapUids = do
        runAppState $ do
          bbAcid <- view gridDBL
          query' bbAcid (GetBoard gId)
            >>= \case
              Nothing -> throw err404
              Just grid -> do
                when (_bb_authorUid grid /= _auser_uid auser) $ throw err403
                now <- getCurrentTime
                update' bbAcid $ UpdateBBTable now $ BBTUUpdateMapPool gId mapUids
          query' bbAcid (GetBoard gId)
            >>= \case
              Nothing -> throw err500
              Just grid -> return grid

  authGrids _ = do
    throwAll err401



collectMapInfo :: Text -> TMMap -> MapInfo
collectMapInfo host tmmap@(TMMap{..})
  = MapInfo
    { _mi_trackId = _tmm_tmxId 
    , _mi_trackUid = _tmm_uid 
    , _mi_trackName = _tmm_name 
    , _mi_authorLogin = fromMaybe "N/A" (_tmm_authorUid )
    , _mi_tags = Text.intercalate "," (show <$> _tmm_tags )
    , _mi_mapType = "TM_Race"
    , _mi_authorTime = _tmm_authorMedal 
    , _mi_wr = fromMaybe (-1) (_tmmr_time <$> _tmm_currentWR )
    , _mi_nbPlayers = fromMaybe (-1) (_tmm_nbPlayers )
    , _mi_isHidden = isJust (_tmm_hiddenReason )
    , _mi_reason = fromMaybe "" (_tmm_hiddenReason )
    , _mi_atSetByPlugin = fromMaybe False (_tmm_atSetByPlugin )
    , _mi_reported = (\(k, (_, r)) -> (k, r)) <$> Map.assocs (_tmm_reportedBy )
    , _mi_uploadedTimestamp = maybe 0 (nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) (_tmm_uploadedAt )
    , _mi_validation =
        case _tmm_validationReplay  of
        Nothing -> (False, "")
        Just (url, _) -> (True, maybe "" (\x -> "https://" <> "map-monitor-replays" <> "." <> host <> "/ghosts/" <> x <> ".Ghost.Gbx") url)
    , _mi_fileSize = fromMaybe (-1) $ _tmm_fileSize 
    , _mi_info = Set.toList $ _tmm_info 

    , _mi_isBeaten = not $ isMapUnbeaten tmmap 
    , _mi_atBeatenBy = _tmmr_userId <$> _tmm_currentWR 
    , _mi_atBeatenTimestamp = _tmmr_timestamp <$> _tmm_currentWR 
    }

collectMapInfoResponse :: Text -> [TMMap] -> MapInfoResponse
collectMapInfoResponse host maps
  = MapInfoResponse
    { _mir_keys = ["TrackID", "TrackUID", "Track_Name", "AuthorLogin", "Tags", "MapType", "AuthorTime", "WR", "NbPlayers", "IsHidden", "Reason", "AtSetByPlugin", "Reported", "UploadedTimestamp", "Validation", "FileSize", "Hints", "IsBeaten", "ATBeatenTimestamp", "ATBeatenUsers"]
    , _mir_tracks = collectMapInfo host <$> maps
    }

trustedUsers :: [Text]
trustedUsers = ["c331bdbf-2182-4a51-813d-87d6f0f209c5", "65ce1935-d166-42b3-89a6-6345ccf41865", "59b84907-59fb-4455-b31d-b0cc44c36ec7", "bce4d579-dc66-43b5-9d57-eb1fb58dd450", "296a77c2-1c19-4236-9a3e-28c8c01e6312", "52f40bee-ef2e-44b9-baf0-067f39dbc45a", "247d5f09-eaa4-4495-9363-b7e69dd42db5", "98b7dfd7-6706-47f4-9b47-6362e9daf7a2", "f520329f-cbb7-45f4-83d3-9b8681a21c6c", "cd81f22a-c92f-48b9-8aa3-72f904c62b66", "c4d583af-15c4-4f6f-8188-a2b6aa0c5e09", "da2b3afe-7610-4d1a-a45c-1430dfbacf86"]

authApiServer :: ServerT AuthAPI AppM
authApiServer = authOpenplanetToken :<|> authIsTrusted :<|> giveAuth
 where
  authOpenplanetToken tok = do
    openplanetSecret <- _settings_openplanetAuthSecret <$> view appSettingsL
    runInClient openPlanetClientL (openPlanetAuthValidate $ OpenPlanetAuthValidateRequest{_opavr_token = (_ia_token tok), _opavr_secret = openplanetSecret})
      >>= \case
        Left err -> do
          print err
          throwError err401
        Right (OpenPlanetAuthValidateResponse{_opavr_account_id = accountId}) -> do
          ajwt <- asks _appState_jwtSettings
          now <- getCurrentTime
          Right newTokBs <- liftIO $ makeJWT (AUser accountId) ajwt (Just $ (secondsToNominalDiffTime $ 60 * 60 * 24) `addUTCTime` now)
          return $ InternalAuth{_ia_token = decodeUtf8 $ toStrictBytes newTokBs}

  authIsTrusted accountId = do
    return $ accountId `elem` trustedUsers

  giveAuth uid = do
    throwError $ err401
    -- ajwt <- asks _appState_jwtSettings
    -- now <- getCurrentTime
    -- Right newTokBs <- liftIO $ makeJWT (AUser uid) ajwt (Just $ (secondsToNominalDiffTime $ 60 * 60 * 24) `addUTCTime` now)
    -- return $ InternalAuth{_ia_token = decodeUtf8 $ toStrictBytes newTokBs}

runAppState :: (MonadReader AppState m, MonadIO m) => ReaderT AppState IO a -> m a
runAppState m = do
  st <- ask
  liftIO $ runReaderT m st

htmxServer :: ServerT HtmxAPI AppM
htmxServer = uploadFile :<|> mapByTmxId
 where
  uploadFile multipartData = do
    resE <- runAppState do
      logInfo $ "Validating replay: " <> displayShow (_vru_tmxid multipartData)
      tryAny do
        host <- view $ appSettingsL . settings_s3_creds . s3_creds_host
        flip finally (removeFile $ _vru_replay $ multipartData) $ do
          s <- flip withAsync wait $ do
            validateReplay multipartData >>= \case
              Left err -> do
                return $ "Validation failed: " <> show err
              Right Nothing -> do
                refreshCaches
                return $ "Validation successful"
              Right (Just uuid) -> do
                refreshCaches
                let url = "https://" <> "map-monitor-replays" <> "." <> host <> "/ghosts/" <> uuid <> ".Ghost.Gbx"
                return $ "Validation successful <a href=\"" <> url <> "\">(ghost)</a>"
          return $ "<div>" <> show (_vru_tmxid multipartData) <> ": " <> s <> "</div>"
    case resE of
      Left err -> do
        logError $ "Error: " <> displayShow err
        return "<div>Error</div>"
      Right res -> do
        logInfo $ "Validation result: " <> displayShow res
        return res

  mapByTmxId tmxidStr = do
    runAppState do
      case readMaybe tmxidStr of
        Nothing -> return ""
        Just tmxid -> do
          dbmapM <- queryAcid $ GetMapById $ TMXId tmxid
          case dbmapM of
            Nothing -> return "Map not found"
            Just dbmap -> return $ "<span>" <> show (unTMXId $ _tmm_tmxId dbmap) <> ": " <> _tmm_name dbmap <> ", " <> show (_tmm_authorMedal dbmap) <> "ms" <> (if isMapUnbeaten dbmap then " (unbeaten)" else " <span style=\"color: yellow;\">(beaten)</span>") <> "</span>"

server1 :: AppState -> FilePath -> ServerT MapMonitorAPI AppM
server1 st staticPath = tmxApiServer :<|> downloadMapsServer st :<|> gridApiServer :<|> managementApiServer :<|> authApiServer :<|> staticServer :<|> htmxServer :<|> dbDump
 where
  staticServer = do
    serveDirectoryWebApp staticPath

  dbDump = do
    db <- queryAcid GetMapMonitorState
    return $
      ExportDBResponse
        { _edr_maps = _mms_maps db @= HiddenOnTmx False
        }

fallbackApp :: Application
fallbackApp _ sendResponse = do
  sendResponse $ responseLBS H.status404 [] ";...;"

app :: Servant.Server.Context '[CookieSettings, JWTSettings, ResponseCache] -> AppState -> Application
app cfg appState inReq sendResponse = do
  let
    staticPath = appState ^. appSettingsL . settings_static
    servantApp =
      serveWithContext mapMonitorAPI cfg $
        hoistServerWithContext
          mapMonitorAPI
          (Proxy :: Proxy '[CookieSettings, JWTSettings, ResponseCache])
          (`runReaderT` appState)
          (server1 appState staticPath)
  -- print $ (rawPathInfo $ inReq) <> (rawQueryString inReq) <> " " <> show (filter (("Authorization" ==) . fst) $ requestHeaders inReq)
  servantApp inReq $ \res ->
    if is404 res
      then fallbackApp inReq sendResponse
      else sendResponse res

is404 :: Wai.Response -> Bool
is404 res =
  case Wai.responseStatus res of
    s -> H.statusCode s == 404

collectBeatenAtsResponse :: (MonadIO m, MonadReader env m, HasState env) => m RecentlyBeatenAtsResponse
collectBeatenAtsResponse = do
  st <- queryAcid GetMapMonitorState
  let
    maps = filter (not . _tmm_hiddenOnTmx) $ filter (not . isMapUnbeaten) $ toDescList (Proxy @WrTimestamp) $ _mms_maps st
    allMaps = take 200 $ maps
    below300k = take 200 $ filter ((< 300000) . _tmm_tmxId) maps
    below200k = take 200 $ filter ((< 200000) . _tmm_tmxId) maps
    below100k = take 200 $ filter ((< 100000) . _tmm_tmxId) maps
  return $
    RecentlyBeatenAtsResponse
      { _rbar_keys = ["TrackID", "TrackUID", "Track_Name", "AuthorLogin", "Tags", "MapType", "AuthorTime", "WR", "LastChecked", "ATBeatenTimestamp", "ATBeatenUsers", "NbPlayers"]
      , _rbar_all = asTracks $ toRow <$> allMaps
      , _rbar_below100k = asTracks $ toRow <$> below100k
      , _rbar_below200k = asTracks $ toRow <$> below200k
      , _rbar_below300k = asTracks $ toRow <$> below300k
      }
 where
  asTracks lst = RecentlyBeatenAtsTracks (length lst) lst
  toRow tmmap =
    ( _tmm_tmxId tmmap
    , _tmm_uid tmmap
    , _tmm_name tmmap
    , fromMaybe "N/A" (_tmm_authorUid tmmap)
    , Text.intercalate "," (show <$> _tmm_tags tmmap)
    , "TM_Race"
    , _tmm_authorMedal tmmap
    , fromMaybe (-1) (_tmmr_time <$> _tmm_currentWR tmmap)
    , fromMaybe (-1) (_tmmr_timestamp <$> _tmm_currentWR tmmap)
    , fromMaybe (-1) (_tmmr_timestamp <$> _tmm_currentWR tmmap)
    , fromMaybe "N/A" (_tmmr_userId <$> _tmm_currentWR tmmap)
    , fromMaybe 123456 (_tmm_nbPlayers tmmap)
    )

-- bingoMapInfo :: (MonadIO m, MonadReader env m, HasState env) => [TMXId] -> m BingoBoardResponse
-- bingoMapInfo mapIds = do
--   st <- queryAcid GetMapMonitorState
--   let
--     maps = filter (not . _tmm_hiddenOnTmx) $ filter (not . isMapUnbeaten) $ toDescList (Proxy @WrTimestamp) $ _mms_maps st @~ mapIds
--   return $
--     RecentlyBeatenAtsResponse
--       { _rbar_keys = ["TrackID", "TrackUID", "Track_Name", "AuthorLogin", "Tags", "MapType", "AuthorTime", "WR", "LastChecked", "ATBeatenTimestamp", "ATBeatenUsers", "NbPlayers", "IsBeaten"]
--       , _rbar_all = asTracks $ toRow <$> allMaps
--       , _rbar_below100k = asTracks $ toRow <$> below100k
--       , _rbar_below200k = asTracks $ toRow <$> below200k
--       , _rbar_below300k = asTracks $ toRow <$> below300k
--       }
--  where
--   asTracks lst = BingoBoardResponse (length lst) lst
--   toRow tmmap =
--     ( _tmm_tmxId tmmap
--     , _tmm_uid tmmap
--     , _tmm_name tmmap
--     , fromMaybe "N/A" (_tmm_authorUid tmmap)
--     , Text.intercalate "," (show <$> _tmm_tags tmmap)
--     , "TM_Race"
--     , _tmm_authorMedal tmmap
--     , fromMaybe (-1) (_tmmr_time <$> _tmm_currentWR tmmap)
--     , fromMaybe (-1) (_tmmr_timestamp <$> _tmm_currentWR tmmap)
--     , fromMaybe (-1) (_tmmr_timestamp <$> _tmm_currentWR tmmap)
--     , fromMaybe "N/A" (_tmmr_userId <$> _tmm_currentWR tmmap)
--     , fromMaybe 123456 (_tmm_nbPlayers tmmap)
--     , not $ isMapUnbeaten tmmap
--     )


collectUnbeatenAtsResponse :: (MonadIO m, MonadReader env m, HasState env, HasAppSettings env) => m (UnbeatenAtsResponse UnbeatenAtTrack)
collectUnbeatenAtsResponse = do
  allMaps <- queryAcid GetMaps
  host <- view $ appSettingsL . settings_s3_creds . s3_creds_host
  let
    unbeatenMaps =
      flip fmap allMaps $ \tmmap ->
        let
          hiddenInfo' =
            ( Text.intercalate "; " $
                catMaybes
                  ( map
                      (\(k, v) -> bool Nothing (Just v) (Set.member k $ _tmm_info tmmap))
                      [ (TMBrokenPhysics, "broken physics")
                      , (TMCheatedAt, "cheated at")
                      ]
                  )
            )
          hiddenInfo = if T.null hiddenInfo' then Nothing else Just hiddenInfo'
         in
          UnbeatenAtTrack
            { _uat_trackId = _tmm_tmxId tmmap
            , _uat_trackUid = _tmm_uid tmmap
            , _uat_trackName = _tmm_name tmmap
            , _uat_authorLogin = fromMaybe "N/A" (_tmm_authorUid tmmap)
            , _uat_tags = Text.intercalate "," (show <$> _tmm_tags tmmap)
            , _uat_mapType = "TM_Race"
            , _uat_authorTime = _tmm_authorMedal tmmap
            , _uat_wr = fromMaybe (-1) (_tmmr_time <$> _tmm_currentWR tmmap)
            , _uat_lastChecked = 0
            , _uat_nbPlayers = fromMaybe 123456 (_tmm_nbPlayers tmmap)
            , _uat_isHidden = isJust (_tmm_hiddenReason tmmap) || isJust hiddenInfo || Just True == _tmm_hasClones tmmap
            , _uat_reason = fromMaybe "" (_tmm_hiddenReason tmmap <|> hiddenInfo <|> bool Nothing (Just "clone car map") (_tmm_hasClones tmmap == Just True))
            , _uat_atSetByPlugin = fromMaybe False (_tmm_atSetByPlugin tmmap)
            , _uat_reported = (\(k, (_, r)) -> (k, r)) <$> Map.assocs (_tmm_reportedBy tmmap)
            , _uat_uploadedTimestamp = maybe 0 (nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) (_tmm_uploadedAt tmmap)
            , _uat_validation =
                case _tmm_validationReplay tmmap of
                  Nothing -> (False, "")
                  Just (url, _) -> (True, maybe "" (\x -> "https://" <> "map-monitor-replays" <> "." <> host <> "/ghosts/" <> x <> ".Ghost.Gbx") url)
            , _uat_fileSize = fromMaybe (-1) $ _tmm_fileSize tmmap
            , _uat_info = Set.toList $ _tmm_info tmmap
            }

  return $
    UnbeatenAtsResponse
      { _uar_keys = ["TrackID", "TrackUID", "Track_Name", "AuthorLogin", "Tags", "MapType", "AuthorTime", "WR", "LastChecked", "NbPlayers", "IsHidden", "Reason", "AtSetByPlugin", "Reported", "UploadedTimestamp", "Validation", "FileSize", "Hints"]
      , _uar_tracks = unbeatenMaps
      , _uar_nbTracks = length unbeatenMaps
      }

collectUnbeatenAtsResponseV2 :: (MonadIO m, MonadReader env m, HasState env, HasAppSettings env) => m (UnbeatenAtsResponse UnbeatenAtTrack)
collectUnbeatenAtsResponseV2 = do
  allMaps <- queryAcid GetMaps
  host <- view $ appSettingsL . settings_s3_creds . s3_creds_host
  let
    unbeatenMaps =
      flip fmap allMaps $ \tmmap ->
        UnbeatenAtTrack
          { _uat_trackId = _tmm_tmxId tmmap
          , _uat_trackUid = _tmm_uid tmmap
          , _uat_trackName = _tmm_name tmmap
          , _uat_authorLogin = fromMaybe "N/A" (_tmm_authorUid tmmap)
          , _uat_tags = Text.intercalate "," (show <$> _tmm_tags tmmap)
          , _uat_mapType = "TM_Race"
          , _uat_authorTime = _tmm_authorMedal tmmap
          , _uat_wr = fromMaybe (-1) (_tmmr_time <$> _tmm_currentWR tmmap)
          , _uat_lastChecked = 0
          , _uat_nbPlayers = fromMaybe (-1) (_tmm_nbPlayers tmmap)
          , _uat_isHidden = isJust (_tmm_hiddenReason tmmap)
          , _uat_reason = fromMaybe "" (_tmm_hiddenReason tmmap)
          , _uat_atSetByPlugin = fromMaybe False (_tmm_atSetByPlugin tmmap)
          , _uat_reported = (\(k, (_, r)) -> (k, r)) <$> Map.assocs (_tmm_reportedBy tmmap)
          , _uat_uploadedTimestamp = maybe 0 (nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) (_tmm_uploadedAt tmmap)
          , _uat_validation =
              case _tmm_validationReplay tmmap of
                Nothing -> (False, "")
                Just (url, _) -> (True, maybe "" (\x -> "https://" <> "map-monitor-replays" <> "." <> host <> "/ghosts/" <> x <> ".Ghost.Gbx") url)
          , _uat_fileSize = fromMaybe (-1) $ _tmm_fileSize tmmap
          , _uat_info = Set.toList $ _tmm_info tmmap
          }

  return $
    UnbeatenAtsResponse
      { _uar_keys = ["TrackID", "TrackUID", "Track_Name", "AuthorLogin", "Tags", "MapType", "AuthorTime", "WR", "LastChecked", "NbPlayers", "IsHidden", "Reason", "AtSetByPlugin", "Reported", "UploadedTimestamp", "Validation", "FileSize", "Hints"]
      , _uar_tracks = unbeatenMaps
      , _uar_nbTracks = length unbeatenMaps
      }

refreshCaches :: (MonadIO m, MonadReader env m, HasResponseCache env) => m ()
refreshCaches = do
  ResponseCache cache <- view responseCacheL
  liftIO $ purge cache
