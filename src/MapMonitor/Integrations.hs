module MapMonitor.Integrations
where

import qualified Data.IxSet.Typed as IxSet
import Data.IxSet.Typed ((@=), (@>))
import Control.Category (id)
import Data.List (last)
import qualified Data.Set as Set
import GHC.Exts
import MapMonitor.API.Nadeo
import MapMonitor.API.TMX
import MapMonitor.API.XertroV
import MapMonitor.ATCheck (checkAtSetByPlugin)
import MapMonitor.Common
import MapMonitor.DB
import Protolude hiding (yield, forkIO, atomically, threadDelay)
import qualified RIO.Map as Map
import RIO.Time
import UnliftIO
import UnliftIO.Concurrent hiding (yield)
import Conduit
import MapMonitor.API.Util
import RIO (logInfo, HasLogFunc, displayShow, logError, logSticky, logStickyDone)
import Data.Conduit.TQueue (sinkTQueue)
import Control.Lens
import MapMonitor.MissingItemsCheck

tmxMapToTMMap :: TMXSearchMapsMap -> TMMap
tmxMapToTMMap tmx =
  TMMap
    { _tmm_tmxId = TMXId $ _tmxsm_MapId tmx
    , _tmm_uid = _tmxsm_MapUid tmx
    , _tmm_name = _tmxsm_Name tmx
    , _tmm_authorMedal = _tmxsm_Author $ _tmxsm_Medals tmx
    , _tmm_authorUid = Nothing
    , _tmm_currentWR = Nothing
    , _tmm_uploadedAt = Nothing
    , _tmm_tags = _tmxsmt_TagId <$> _tmxsm_Tags tmx
    , _tmm_hiddenReason = Nothing
    , _tmm_atSetByPlugin = Nothing
    , _tmm_nbPlayers = Nothing
    , _tmm_reportedBy = mempty
    , _tmm_mapType = case _tmxsm_MapType tmx of
        "TM_Race" -> Just MT_Race
        "TM_Royal" -> Just MT_Royal
        "TM_Stunt" -> Just MT_Stunt
        "TM_Platform" -> Just MT_Platform
        "Puzzle" -> Just MT_Puzzle
        x -> Just $ MT_Other x
    , _tmm_mapVersions = []
    , _tmm_hiddenOnTmx = False
    }

tmxMapsSource :: (MonadIO m, MonadReader env m, HasTMXClient env, HasLogFunc env) => Int -> Maybe Int -> ConduitT () TMMap m ()
tmxMapsSource chnkSize after = do
  respE <- runInClient tmxClientL $ tmxSearchMaps $ TMXSearchMaps{_tmxsm_ids = [], _tmxsm_count = Just (chnkSize `min` 200), _tmxsm_after = after, _tmxsm_from = Nothing, _tmxsm_order1 = Just 6}

  case respE of
    Left err -> do
      logError $ "Error: " <> displayShow err
    Right resp -> do
      logInfo $ "Got " <> displayShow (length $ _tmxsr_Results resp) <> " maps"
      yieldMany $ tmxMapToTMMap <$> _tmxsr_Results resp
      threadDelay (500 * 1000)
      when (_tmxsr_More resp) $ do
        tmxMapsSource chnkSize (Just $ _tmxsm_MapId $ last $ _tmxsr_Results resp)

scanTmx :: (MonadReader env m, HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasState env, HasTMXClient env, MonadFail m, MonadUnliftIO m, HasNadeoAuthToken env, HasCheckMapFileQueue env) => Maybe Int -> m ()
scanTmx cntM = do
  logInfo $ "rescanning last " <> displayShow cntM <> " maps from TMX"
  runConduit $
    tmxMapsSource 200 Nothing
    .| maybe (mapC id) takeC cntM
    .| collectUnknownMapC

collectUnknownMapC :: (MonadIO m, MonadFail m, MonadReader env m, HasState env,  HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env,  HasNadeoLiveClient env, HasNadeoRequestRate env,  HasNadeoThrottler env, HasNadeoAuthToken env, HasCheckMapFileQueue env) => ConduitT TMMap c m ()
collectUnknownMapC = do
  checkMapFileQueue <- view checkMapFileQueueL

  filterMC (fmap not . queryAcid . IsKnownId . _tmm_tmxId)
    .| wrapChunkedC 100 (iterMC $ updateAcid . AddNewMaps')
    .| loadNadeoMapInfoC
    .| refreshMapRecordC Nothing
    .| filterC isMapUnbeaten
    .| sinkTQueue checkMapFileQueue

loadNadeoMapInfoC :: (MonadIO m, MonadFail m, MonadReader env m, HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasCallStack, HasNadeoAuthToken env, HasState env) => ConduitT TMMap TMMap m ()
loadNadeoMapInfoC = nadeoMapInfoC .| mapMC (withAcid1 updateMaps . fmap fst) .| concatC

indexedC :: (Monad m) => Int -> ConduitT a (Int, a) m ()
indexedC start = do
  let loop i = do
        xM <- await
        case xM of
          Nothing -> return ()
          Just x -> do
            yield (i, x)
            loop (i + 1)
  loop start


refreshMapRecordC :: (MonadIO m, MonadReader env m, HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, MonadFail m, HasNadeoAuthToken env, HasState env) => Maybe Int -> ConduitT TMMap TMMap m ()
refreshMapRecordC mapsCntM =
  indexedC 0
  .| mapMC (\(i, tmmap) -> do
      logInfo $ "Pulling records for map #" <> displayShow i <> "/" <> maybe "?" displayShow mapsCntM <> ": " <> displayShow (unTMXId (_tmm_tmxId tmmap), _tmm_uid tmmap)
      fst <$> getMapRecord tmmap
  )
  .| wrapChunkedC 100 (mapMC $ withAcid1 updateMaps)

-- mapHasNewInfoFilterMC :: (MonadUnliftIO m, MonadReader env m, HasState env) => ConduitT TMMap TMMap m ()
-- mapHasNewInfoFilterMC = do
--   mapMC (\mp -> (,) <$> checkMapHasNewInfo mp <*> pure mp)
--   .| filterC fst
--   .| mapC snd
--
--   where
--     checkMapHasNewInfo mp = do
--       dbmapM <- queryAcid $ GetMapById (_tmm_tmxId mp)
--       case dbmapM of
--         Nothing -> return True
--         Just dbmap -> do
--           return $ or
--             [ _tmm_uid dbmap /= _tmm_uid mp
--             , _tmm_name dbmap /= _tmm_name mp
--             , _tmm_authorMedal dbmap /= _tmm_authorMedal mp
--             , _tmm_tags dbmap /= _tmm_tags mp
--             , _tmm_mapType dbmap /= _tmm_mapType mp
--             ]

getMapRecord :: (MonadIO m, MonadReader env m, HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, MonadFail m, HasNadeoAuthToken env) => TMMap -> m (TMMapPatch, TMMap)
getMapRecord tmmap = do
  let mapPatch = defPatch $ _tmm_tmxId tmmap
  nadeoGetMapLeaderboard (_tmm_uid tmmap) 1
    >>= \case
    Left err -> do
      logError $ "Error: " <> displayShow err
      return (mapPatch, tmmap)
    Right (GetMapLeaderboardResponse{_gmlr_tops = [GetMapLeaderboardTop{_gmlt_top = [topRecord]}]}) -> do
      logInfo $ "Got record for map " <> displayShow (unTMXId $ _tmm_tmxId tmmap, _tmm_uid tmmap)
      return $ (mapPatch{_tmmp_currentWR = Just $ Just (TMMapRecord (_gmlte_accountId topRecord) (_gmlte_score topRecord) (_gmlte_timestamp topRecord))}, tmmap)
    err@_ -> do
      logInfo $ "No records found for map " <> displayShow (_tmm_uid tmmap) <> " with error: " <> displayShow err
      return (mapPatch, tmmap)


chunkedC :: (Monad m) => Int -> ConduitT a [a] m ()
chunkedC chnkSize = do
  let
    loop = do
      lst <- takeC chnkSize .| sinkList
      if null lst
         then return ()
         else do
           yield lst
           loop
  loop

wrapChunkedC :: (Monad m) => Int -> ConduitT [a] [b] m () -> ConduitT a b m ()
wrapChunkedC chnkSize conduit = do
  chunkedC chnkSize
  .| conduit
  .| concatC

nadeoMapInfoC :: (MonadIO m, MonadFail m, MonadReader env m, HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasCallStack, HasNadeoAuthToken env         ) => ConduitT TMMap [(TMMapPatch, TMMap)] m ()
nadeoMapInfoC = do
  chunkedC 100
    .| mapMC \tmmaps -> do
      logInfo $ "Pulling info for maps " <> displayShow (unTMXId . _tmm_tmxId $ fromMaybe undefined $ head tmmaps) <> "-" <> displayShow (unTMXId . _tmm_tmxId $ last tmmaps)
      updates <- nadeoGetMapMultiple (IdsList $ fmap _tmm_uid tmmaps)
        >>= \case
        Left err -> do
          logError $ "Error: " <> displayShow err
          return mempty
        Right res -> do
          return $ Map.fromList $ (\mp -> (_gmmrm_uid mp, mp)) <$> (_gmmr_mapList res)
      return $
        [ ( case mpM of
            Nothing -> patch
            Just mp -> patch
              { _tmmp_authorUid = Just $ Just (_gmmrm_author mp)
                , _tmmp_uploadedAt = Just $ Just (unUTCTimestamp $ _gmmrm_uploadTimestamp mp)
              }
          , tmmap
          )
          | tmmap <- tmmaps
          , let mpM = Map.lookup (_tmm_uid tmmap) updates
          , let patch = defPatch $ _tmm_tmxId tmmap
        ]

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked n xs = take n xs : chunked n (drop n xs)

getTmxMapC :: (MonadReader env m, HasTMXClient env, HasLogFunc env, MonadIO m  ) => ConduitT TMXId [TMMap] m ()
getTmxMapC = do
  chunkedC 100
    .| mapMC \ids -> do
      logInfo $ "Pulling info for maps " <> displayShow (minimum ids) <> "-" <> displayShow (maximum ids)
      threadDelay (1000 * 1000)
      runInClient tmxClientL (tmxSearchMaps TMXSearchMaps{_tmxsm_ids = (unTMXId <$> ids), _tmxsm_count = Just 100, _tmxsm_after = Nothing, _tmxsm_from = Nothing, _tmxsm_order1 = Nothing})
        >>= \case
          Left err -> do
            logError $ "Error: " <> displayShow err
            return []
          Right res -> do
            logInfo $ "Got " <> displayShow (length $ _tmxsr_Results res) <> " maps"
            return $ tmxMapToTMMap <$> _tmxsr_Results res

refreshBeatenMaps :: (MonadReader env m) =>m ()
refreshBeatenMaps = do
  -- WTF do I do now xpp
  pass
  -- beaten <- updateAcid ShuffleBeatenMaps
  -- void $ forkIO do
  --   forM_ beaten \tmmap -> do
  --     case tmmap of
  --       TMMap {_tmm_currentWR = Just wr} -> do
  --         rpcSend $
  --           PMMapBeatenPing $
  --             MapBeatenPing
  --             { _mbp_tmxId = unTMXId $ _tmm_tmxId tmmap
  --             , _mbp_uid = _tmm_uid tmmap
  --             , _mbp_authorUid = fromMaybe "" $ _tmm_authorUid tmmap
  --             , _mbp_name = _tmm_name tmmap
  --             , _mbp_wrHolder = _tmmr_userId wr
  --             , _mbp_wrTime = _tmmr_time wr
  --             , _mbp_wrTimestamp = posixSecondsToUTCTime $ fromIntegral $ _tmmr_timestamp wr
  --             }
  --       _ -> logError $ "Got unbeaten map marked as beaten? " <> displayShow tmmap
  -- logInfo $ displayShow (length beaten) <> " maps were beaten"

addMissingMaps :: (MonadReader env m, HasTMXClient env, HasState env, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasNadeoCoreClient env, MonadUnliftIO m, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env, HasNadeoAuthToken env, HasCheckMapFileQueue env) => [Int] -> m ()
addMissingMaps [] = pass
addMissingMaps ids = do
  knownIds <- queryAcid GetAllKnownIds
  let newMaps = fromList ids `Set.difference` knownIds
  unless (null newMaps) do
    runConduit $
      yieldMany ids
      .| mapC TMXId
      .| getTmxMapC
      .| concatC
      .| collectUnknownMapC

refreshMissingInfo :: (MonadIO m, MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasNadeoThrottler env, HasNadeoRequestRate env, HasLogFunc env, HasNadeoAuthToken env) => m ()
refreshMissingInfo = do
  maps <- filterMaps (@= HasNadeoInfo False)
  logInfo $ "Reloading missing info for " <> displayShow (length maps) <> " maps."
  runConduit $
    yieldMany maps
    .| loadNadeoMapInfoC
    .| sinkNull

refreshRecentUnbeatenMaps :: (MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env, HasNadeoAuthToken env, MonadUnliftIO m) =>m ()
refreshRecentUnbeatenMaps = do
  now <- getCurrentTime
  let last7Days = addUTCTime (negate $ secondsToNominalDiffTime $ 60 * 60 * 24 * 7) now
  maps <- filterMaps ((@> UploadedAt last7Days) . (@= HasNadeoInfo True) . (@= Unbeaten))
  logInfo $ "Refreshing records on recently uploaded unbeaten maps: " <> displayShow (length maps)
  runConduit $
    yieldMany maps
    .| refreshMapRecordC (Just $ length maps)
    .| sinkNull

refreshUnbeatenMaps :: (MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env, HasNadeoAuthToken env, MonadUnliftIO m) =>m ()
refreshUnbeatenMaps = do
  logInfo "Refreshing records on all unbeaten maps"
  maps <- filterMaps ((@= HasNadeoInfo True) . (@= Unbeaten))
  runConduit $
    yieldMany maps
    .| refreshMapRecordC (Just $ length maps)
    .| sinkNull

refreshNbPlayers :: (MonadReader env m, HasState env, HasXertrovClient env, MonadUnliftIO m, HasLogFunc env) => m ()
refreshNbPlayers = do
  maps <- filter (isJust . _tmm_authorUid) <$> queryAcid GetMaps
  flip (pooledMapConcurrentlyN_ 4) maps \tmmap -> do
    runInClient xertrovClientL (xertrovGetNbPlayers (_tmm_uid tmmap)) >>= \case
      Left err -> do
        logError $ displayShow (_tmm_tmxId tmmap) <> ": error: " <> displayShow err
      Right res -> do
        logInfo $ displayShow (_tmm_tmxId tmmap) <> ": got nb players: " <> displayShow (_xmpr_nb_players res)
        void $ withAcid1 updateMaps [(defPatch (_tmm_tmxId tmmap)) {_tmmp_nbPlayers = Just $ Just (_xmpr_nb_players res)}]

rescanMaps :: (MonadReader env m, HasState env, MonadUnliftIO m, HasAppSettings env, HasLogFunc env) => m ()
rescanMaps = do
  maps <- filter (isNothing . _tmm_hiddenReason) <$> queryAcid GetMaps
  pooledForConcurrentlyN_ 10 (zip [(1 :: Int) ..] maps) $ \(i, tmmap) -> do
    logSticky $ "Checking map #" <> displayShow i <> "/" <> displayShow (length maps) <> ": " <> displayShow (_tmm_tmxId tmmap)
    -- case tmmap of
    --   TMMap{_tmm_atSetByPlugin = Nothing, _tmm_tmxId = tmxId} -> do
    --     atSetByPlugin <- checkAtSetByPlugin $ unTMXId tmxId
    --     logInfo $ "Checking AT set by plugin for map " <> displayShow tmxId <> ": " <> displayShow atSetByPlugin
    --     if isJust atSetByPlugin
    --       then updateAcid (SetAtSetByPlugin tmxId atSetByPlugin)
    --       else pass
    --   _ -> pass
  -- forM_ unbeatenMaps $ \tmmap -> do
    checkMissingItems (unTMXId $ _tmm_tmxId tmmap)
      >>= \case
        Just MissingItems -> do
          logStickyDone $ "Checking map #" <> displayShow i <> "/" <> displayShow (length maps) <> ": " <> displayShow (_tmm_tmxId tmmap)
          putText $ "Missing items detected for map " <> show (_tmm_tmxId tmmap)
          updateAcid $ HideMap (_tmm_tmxId tmmap) "Missing items detected by automatic check"
          -- q <- view beatenMapPingsL
          -- atomically $ writeTQueue q $
          --   PMMissingItemsMapDetectedPing $
          --     MissingItemsMapDetectedPing
          --       { _mimdp_tmxId = unTMXId $ _tmm_tmxId tmmap
          --       , _mimdp_uid = _tmm_uid tmmap
          --       , _mimdp_name = _tmm_name tmmap
          --       , _mimdp_authorUid = fromMaybe "" $ _tmm_authorUid tmmap
          --       }
        _ -> pass

processAtCheckQueue :: (MonadUnliftIO m, MonadReader env m, HasAppSettings env,  HasLogFunc env, HasState env) => TQueue TMMap -> m ()
processAtCheckQueue queue = do
  atomically (readTQueue queue) >>= \tmmap -> do
    atSetByPlugin <- checkAtSetByPlugin (unTMXId $ _tmm_tmxId tmmap)
    logInfo $ "Checking AT set by plugin for map #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> ": " <> displayShow atSetByPlugin
    Protolude.void $ withAcid1 updateMaps $ [(defPatch $ _tmm_tmxId tmmap){_tmmp_atSetByPlugin = Just atSetByPlugin}]

filterMaps :: (MonadIO m, MonadReader env m, HasState env) => (IxEntry -> IxEntry) -> m [TMMap]
filterMaps f = do
  st <- queryAcid GetMapMonitorState
  return $ IxSet.toList $ f $ _mms_maps st
