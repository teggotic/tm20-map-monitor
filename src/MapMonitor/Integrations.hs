module MapMonitor.Integrations
where

import Data.List (last)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
import Servant.Client
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import UnliftIO
import UnliftIO.Concurrent hiding (yield)
import Conduit
import MapMonitor.API.Util
import RIO (logInfo, HasLogFunc, displayShow, logError, logSticky, logStickyDone)
import Data.Conduit.TQueue (sinkTQueue)
import PingRPC
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
    }

iterTmxMaps :: (MonadIO m, MonadReader env m, HasTMXClient env, HasLogFunc env) => Int -> Maybe Int -> ConduitT () TMMap m ()
iterTmxMaps chnkSize after = do
  respE <- runInClient tmxClientL $ tmxSearchMaps $ TMXSearchMaps{_tmxsm_ids = [], _tmxsm_count = Just (chnkSize `min` 200), _tmxsm_after = after, _tmxsm_from = Nothing, _tmxsm_order1 = Just 6}

  case respE of
    Left err -> do
      logError $ "Error: " <> displayShow err
    Right resp -> do
      logInfo $ "Got " <> displayShow (length $ _tmxsr_Results resp) <> " maps"
      yieldMany $ tmxMapToTMMap <$> _tmxsr_Results resp
      threadDelay (500 * 1000)
      when (_tmxsr_More resp) $ do
        iterTmxMaps chnkSize (Just $ _tmxsm_MapId $ last $ _tmxsr_Results resp)

fullRescan :: (MonadReader env m, HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasState env, HasTMXClient env, MonadFail m, MonadUnliftIO m, HasNadeoAuthToken env) => TQueue TMMap -> m ()
fullRescan checkAtQueue = do
  ids <- getAllKnownIds
  runConduit $
    iterTmxMaps 200 Nothing
    .| filterC (\tmmap -> not $ unTMXId (_tmm_tmxId tmmap) `elem` ids)
    .| loadNadeoMapsInfoC
    .| concatC
    .| mapC (uncurry applyPatch)
    .| refreshRecordsC
    .| mapC (uncurry applyPatch)
    .| iterMC (\tmmap -> updateAcid $ AddNewMaps [(_tmm_tmxId tmmap, tmmap)])
    -- .| mapMC (\m -> Prelude.head <$> withAcid1 updateMaps [m])
    .| filterC isMapUnbeaten
    .| sinkTQueue checkAtQueue

refreshRecordsC :: (MonadIO m, MonadReader env m, HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, MonadFail m, HasNadeoAuthToken env) => ConduitT TMMap (TMMapPatch, TMMap) m ()
refreshRecordsC = do
  mapMC \tmmap -> do
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

loadNadeoMapsInfoC :: (MonadIO m, MonadFail m, MonadReader env m, HasLogFunc env, HasNadeoCoreClient env, HasNadeoTokenState env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasCallStack, HasNadeoAuthToken env         ) => ConduitT TMMap [(TMMapPatch, TMMap)] m ()
loadNadeoMapsInfoC = do
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
    -- .| concatC

recheckTmxForLatestMissingMaps cnt = recheckTmxForMissingMaps cnt Nothing

recheckTmxForMissingMaps :: (MonadReader env m, HasState env, HasTMXClient env,  HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env,  HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env, HasNadeoAuthToken env, HasPubRpcSocket env) => Int -> Maybe Int -> m (Maybe TMXId)
recheckTmxForMissingMaps cnt frm = do
  logError $ "rescanning last " <> displayShow cnt <> " maps from TMX"
  ids <- getAllKnownIds
  maps <- runConduit $ iterTmxMaps cnt frm .| takeC cnt .|  filterC (\tmmap -> not $ unTMXId (_tmm_tmxId tmmap) `elem` ids) .| sinkList
  collectUnknownMaps maps
  return $ _tmm_tmxId <$> lastMay maps

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked n xs = take n xs : chunked n (drop n xs)

pullKnownMapsC :: (MonadReader env m, HasTMXClient env, HasLogFunc env, MonadIO m  ) => ConduitT TMXId [TMMap] m ()
pullKnownMapsC = do
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

pullKnownMaps :: [Int] -> ClientM [TMXSearchMapsMap]
pullKnownMaps [] = return []
pullKnownMaps frm = do
  let ids = take 100 frm
  res <- tmxSearchMaps $ TMXSearchMaps{_tmxsm_ids = ids, _tmxsm_count = Just 100, _tmxsm_after = Nothing, _tmxsm_from = Nothing, _tmxsm_order1 = Nothing}
  putText $ "Got " <> show (length $ _tmxsr_Results res) <> " maps, " <> show (length frm - (length $ _tmxsr_Results res)) <> " left"
  -- return $ _tmxsr_Results res
  let moreIds = drop 100 frm
  if null moreIds
    then return $ _tmxsr_Results res
    else do
      threadDelay (1000 * 1000)
      more <- pullKnownMaps moreIds
      return $ _tmxsr_Results res ++ more

loadNadeoMapsInfo :: (MonadIO m, HasNadeoLiveClient env, MonadReader env m, MonadFail m, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoThrottler env, HasNadeoRequestRate env, HasLogFunc env, HasNadeoAuthToken env) => [TMMap] -> m [TMMapPatch]
loadNadeoMapsInfo tmmaps = do
  let cnt = length tmmaps
  logInfo $ "Loading info for " <> displayShow cnt <> " maps"
  let uidDict = fromList $ fmap (\tmmap -> (_tmm_uid tmmap, defPatch $ _tmm_tmxId tmmap)) tmmaps
  updates <- forM (zip [(1::Int)..] $ chunked 100 tmmaps) $ \(i, maps) -> do
    logInfo $ "Pulling info for maps #" <> displayShow i <> "/" <> displayShow (cnt `quot` 100)
    res <- nadeoGetMapMultiple (IdsList $ fmap _tmm_uid maps)
    case res of
      Left err -> do
        logError $ "Error: " <> displayShow err
        return []
      Right res -> do
        -- print res
        return $ (\mp -> (mp, _gmmrm_uid mp)) <$> (_gmmr_mapList res)
  let
    xx =
      foldl'
        ( \acc (mp, uid) ->
            Map.adjust
              ( \patch ->
                  patch
                    { _tmmp_authorUid = Just $ Just (_gmmrm_author mp)
                    , _tmmp_uploadedAt = Just $ Just (unUTCTimestamp $ _gmmrm_uploadTimestamp mp)
                    }
              )
              uid
              acc
        )
        uidDict
        (concat updates)
  return $ Map.elems xx

refreshRecords :: (MonadIO m, MonadReader env m, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoThrottler env, HasNadeoRequestRate env, HasLogFunc env, HasNadeoAuthToken env) => [TMMap] -> m [TMMapPatch]
refreshRecords maps = do
  start <- liftIO $ getTime Monotonic
  logInfo $ "Refreshing records: " <> displayShow start
  forM (zip [(1 :: Int) ..] maps) \(i, tmmap) -> do
    let mapPatch = defPatch $ _tmm_tmxId tmmap
    now <- liftIO $ getTime Monotonic
    let diffNs = toNanoSecs (diffTimeSpec now start)
        diffSec = fromIntegral diffNs / 1e9 :: Double
    logInfo $ "Pulling records for map #" <> displayShow i <> "/" <> displayShow (length maps) <> ": " <> displayShow (unTMXId (_tmm_tmxId tmmap), _tmm_uid tmmap) <> " (" <> displayShow diffSec <> "s)"
    records <- nadeoGetMapLeaderboard (_tmm_uid tmmap) 1
    updatedMap <- case records of
      Left err -> do
        logError $ "Error: " <> displayShow err
        return mapPatch
      Right (GetMapLeaderboardResponse{_gmlr_tops = [GetMapLeaderboardTop{_gmlt_top = [topRecord]}]}) -> do
        logInfo $ "Got record for map " <> displayShow (_tmm_uid tmmap)
        return $ mapPatch{_tmmp_currentWR = Just $ Just (TMMapRecord (_gmlte_accountId topRecord) (_gmlte_score topRecord) (_gmlte_timestamp topRecord))}
      err@_ -> do
        logInfo $ "No records found for map " <> displayShow (_tmm_uid tmmap) <> " with error: " <> displayShow err
        return mapPatch
    return updatedMap

refreshBeatenMaps :: ( MonadReader env m, HasState env, HasLogFunc env, MonadUnliftIO m, HasPubRpcSocket env) =>m ()
refreshBeatenMaps = do
  beaten <- updateAcid ShuffleBeatenMaps
  void $ forkIO do
    forM_ beaten \tmmap -> do
      case tmmap of
        TMMap {_tmm_currentWR = Just wr} -> do
          rpcSend $
            PMMapBeatenPing $
              MapBeatenPing
              { _mbp_tmxId = unTMXId $ _tmm_tmxId tmmap
              , _mbp_uid = _tmm_uid tmmap
              , _mbp_authorUid = fromMaybe "" $ _tmm_authorUid tmmap
              , _mbp_name = _tmm_name tmmap
              , _mbp_wrHolder = _tmmr_userId wr
              , _mbp_wrTime = _tmmr_time wr
              , _mbp_wrTimestamp = posixSecondsToUTCTime $ fromIntegral $ _tmmr_timestamp wr
              }
        _ -> logError $ "Got unbeaten map marked as beaten? " <> displayShow tmmap
  logInfo $ displayShow (length beaten) <> " maps were beaten"

getAllKnownIds :: (MonadReader env m, MonadIO m, HasState env) => m (Set Int)
getAllKnownIds = do
  st <- queryAcid GetMapMonitorState
  return $ fromList $ fmap (unTMXId . _tmm_tmxId) $ (Map.elems $ mms_ubeatenMaps st) <> mms_beatenMaps st

collectUnknownMaps :: (MonadReader env m, HasState env, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env, HasNadeoAuthToken env, HasPubRpcSocket env) => [TMMap] -> m ()
collectUnknownMaps [] = pass
collectUnknownMaps newMaps = do
  logInfo $ "Adding new maps: " <> displayShow (length newMaps)
  updateAcid (AddNewMaps (zip (_tmm_tmxId <$> newMaps) newMaps))

  mapsWithUserInfo <- withAcid1 updateMaps =<< loadNadeoMapsInfo newMaps

  mapsWithRecords <- withAcid1 updateMaps =<< refreshRecords (filter (isJust . _tmm_authorUid) mapsWithUserInfo)

  let unbeatenMaps = (filter isMapUnbeaten mapsWithRecords)
  unless (null unbeatenMaps) $ do
    void $ forkIO $ void do
      logInfo $ "Checking AT set by plugin for unbeaten maps: " <> displayShow (length unbeatenMaps)
      _ <- withAcid1 updateMaps
        =<< flip (pooledMapConcurrentlyN 2) (zip [(1::Int)..] unbeatenMaps) \(i, tmmap) -> do
          atSetByPlugin <- checkAtSetByPlugin (unTMXId $ _tmm_tmxId tmmap)
          logInfo $ "Checking AT set by plugin for map #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> "(" <> displayShow i <> "/" <> displayShow (length unbeatenMaps) <> "): " <> displayShow atSetByPlugin
          return $ (defPatch $ _tmm_tmxId tmmap){_tmmp_atSetByPlugin = Just atSetByPlugin}

      forM_ unbeatenMaps $ \tmmap -> do
        checkMissingItems (unTMXId $ _tmm_tmxId tmmap)
          >>= \case
            Just MissingItems -> do
              updateAcid $ HideMap (_tmm_tmxId tmmap) "Missing items detected by automatic check"
              rpcSend $
                PMMissingItemsMapDetectedPing $
                  MissingItemsMapDetectedPing
                    { _mimdp_tmxId = unTMXId $ _tmm_tmxId tmmap
                    , _mimdp_uid = _tmm_uid tmmap
                    , _mimdp_name = _tmm_name tmmap
                    , _mimdp_authorUid = fromMaybe "" $ _tmm_authorUid tmmap
                    }
            _ -> pass

  refreshBeatenMaps

addMissingMaps :: (MonadReader env m, HasTMXClient env, HasState env, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env, HasNadeoAuthToken env, HasPubRpcSocket env) => [Int] -> m ()
addMissingMaps [] = pass
addMissingMaps ids = do
  knownIds <- getAllKnownIds
  let newMaps = fromList ids `Set.difference` knownIds
  if null newMaps
    then pass
    else do
      resE <- runInClient tmxClientL $ pullKnownMaps ids
      case resE of
        Left err -> logError $ "Error: " <> displayShow err
        Right res -> do
          logInfo $ "Got " <> displayShow (length res) <> " maps"
          let maps = tmxMapToTMMap <$> res
          print maps

          collectUnknownMaps maps

refreshMissingInfo :: (MonadIO m, MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasNadeoThrottler env, HasNadeoRequestRate env, HasLogFunc env, HasNadeoAuthToken env) => m ()
refreshMissingInfo = do
  maps <- filter (isNothing . _tmm_authorUid) <$> queryAcid GetMaps
  logInfo $ "Reloading missing info for maps: " <> displayShow (length maps)
  void $ withAcid1 updateMaps =<< loadNadeoMapsInfo maps

refreshRecentUnbeatenMaps :: ( MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env, HasNadeoAuthToken env, MonadUnliftIO m, HasPubRpcSocket env) =>m ()
refreshRecentUnbeatenMaps = do
  logInfo "Refreshing records on recently uploaded unbeaten maps"
  now <- getCurrentTime
  let last7Days = addUTCTime (negate $ secondsToNominalDiffTime $ 60 * 60 * 24 * 7) now
  let uploadedRecently (_tmm_uploadedAt -> Just uploadedAt) = uploadedAt > last7Days
      uploadedRecently _ = False

  maps <- filter (liftA2 (&&) (isJust . _tmm_authorUid) uploadedRecently) <$> queryAcid GetMaps
  logInfo $ "Unbeaten maps uploaded recently: " <> displayShow (length maps)
  void $ withAcid1 updateMaps =<< refreshRecords maps

  refreshBeatenMaps

refreshUnbeatenMaps :: ( MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env, HasNadeoAuthToken env, MonadUnliftIO m, HasPubRpcSocket env) =>m ()
refreshUnbeatenMaps = do
  logInfo "Refreshing records on all unbeaten maps"
  maps <- filter (isJust . _tmm_authorUid) <$> queryAcid GetMaps

  void $ withAcid1 updateMaps =<< refreshRecords maps

  refreshBeatenMaps

refreshNbPlayers :: (MonadReader env m, HasState env, HasXertrovClient env, MonadUnliftIO m, HasLogFunc env) => m ()
refreshNbPlayers = do
  maps <- filter (isJust . _tmm_authorUid) <$> queryAcid GetMaps
  flip (pooledMapConcurrentlyN_ 4) maps \tmmap -> do
    runInClient xertrovClientL (xertrovGetNbPlayers (_tmm_uid tmmap)) >>= \case
      Left err -> do
        logError $ displayShow (_tmm_tmxId tmmap) <> ": error: " <> displayShow err
      Right res -> do
        logInfo $ displayShow (_tmm_tmxId tmmap) <> ": got nb players: " <> displayShow (_xmpr_nb_players res)
        updateAcid (ReplaceMap tmmap{_tmm_nbPlayers = Just (_xmpr_nb_players res)})

rescanMaps :: (MonadReader env m, HasState env, MonadUnliftIO m, HasAppSettings env, HasLogFunc env, HasPubRpcSocket env) => m ()
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

processAtCheckQueue queue = do
  atomically (readTQueue queue) >>= \tmmap -> do
    atSetByPlugin <- checkAtSetByPlugin (unTMXId $ _tmm_tmxId tmmap)
    logInfo $ "Checking AT set by plugin for map #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> ": " <> displayShow atSetByPlugin
    Protolude.void $ withAcid1 updateMaps $ [(defPatch $ _tmm_tmxId tmmap){_tmmp_atSetByPlugin = Just atSetByPlugin}]
