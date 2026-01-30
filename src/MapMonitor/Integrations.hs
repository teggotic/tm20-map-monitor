module MapMonitor.Integrations
where

import Data.List (last)
import qualified Data.Set as Set
import GHC.Exts
import MapMonitor.API.Nadeo
import MapMonitor.API.TMX
import MapMonitor.API.XertroV
import MapMonitor.ATCheck (checkAtSetByPlugin)
import MapMonitor.Common
import MapMonitor.DB
import Protolude hiding (forkIO, atomically, threadDelay)
import qualified RIO.Map as Map
import RIO.Time
import Servant.Client
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import UnliftIO
import UnliftIO.Concurrent
import Conduit
import MapMonitor.API.Util
import RIO (logInfo, HasLogFunc, displayShow, logError)

iterTmxMaps :: (MonadIO m, MonadReader env m, HasTMXClient env, HasLogFunc env) => Int -> Maybe Int -> ConduitT () TMMap m ()
iterTmxMaps cnt after = do
  respE <- runInClient tmxClientL $ tmxSearchMaps $ TMXSearchMaps{_tmxsm_ids = [], _tmxsm_count = Just (cnt `min` 200), _tmxsm_after = after, _tmxsm_from = Nothing, _tmxsm_order1 = Just 6}

  case respE of
    Left err -> do
      logError $ "Error: " <> displayShow err
    Right resp -> do
      logInfo $ "Got " <> displayShow (length $ _tmxsr_Results resp) <> " maps"
      yieldMany $ tmxMapToTMMap <$> _tmxsr_Results resp
      when (_tmxsr_More resp) $ do
        iterTmxMaps cnt (Just $ _tmxsm_MapId $ last $ _tmxsr_Results resp)

fullRescan frm = do
  recheckTmxForMissingMaps 1000 (Just frm)
    >>= \case
      Nothing -> pass
      Just (unTMXId -> lastId) -> do
        fullRescan lastId

recheckTmxForLatestMissingMaps cnt = recheckTmxForMissingMaps cnt Nothing

recheckTmxForMissingMaps :: (MonadReader env m, HasState env, HasTMXClient env,  HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env,  HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env) => Int -> Maybe Int -> m (Maybe TMXId)
recheckTmxForMissingMaps cnt frm = do
  logError $ "rescanning last " <> displayShow cnt <> " maps from TMX"
  ids <- getAllKnownIds
  maps <- runConduit $ iterTmxMaps cnt frm .| takeC cnt .|  filterC (\tmmap -> not $ unTMXId (_tmm_tmxId tmmap) `elem` ids) .| sinkList
  collectUnknownMaps maps
  return $ _tmm_tmxId <$> lastMay maps

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked n xs = take n xs : chunked n (drop n xs)

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

loadNadeoMapsInfo :: (MonadIO m, HasNadeoLiveClient env, MonadReader env m, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env, HasNadeoThrottler env, HasNadeoRequestRate env, HasLogFunc env) => [TMMap] -> m [TMMapPatch]
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

refreshRecords :: (MonadIO m, MonadReader env m, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env, HasNadeoThrottler env, HasNadeoRequestRate env, HasLogFunc env) => [TMMap] -> m [TMMapPatch]
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

refreshBeatenMaps :: (MonadIO m, MonadReader env m, HasState env, HasLogFunc env) => m ()
refreshBeatenMaps = do
  mapsBeforeShuffle <- length <$> queryAcid GetMaps
  updateAcid ShuffleBeatenMaps
  mapsAfterShuffle <- length <$> queryAcid GetMaps
  logInfo $ displayShow (mapsBeforeShuffle - mapsAfterShuffle) <> " maps were beaten"

getAllKnownIds :: (MonadReader env m, MonadIO m, HasState env) => m (Set Int)
getAllKnownIds = do
  st <- queryAcid GetMapMonitorState
  return $ fromList $ fmap (unTMXId . _tmm_tmxId) $ (Map.elems $ mms_ubeatenMaps st) <> mms_beatenMaps st

collectUnknownMaps :: (MonadReader env m, HasState env, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env) => [TMMap] -> m ()
collectUnknownMaps [] = pass
collectUnknownMaps newMaps = do
  updateAcid (AddNewMaps (zip (_tmm_tmxId <$> newMaps) newMaps))

  mapsWithUserInfo <- withAcid1 updateMaps =<< loadNadeoMapsInfo newMaps

  mapsWithRecords <- withAcid1 updateMaps =<< refreshRecords (filter (isJust . _tmm_authorUid) mapsWithUserInfo)

  let unbeatenMaps = (filter isMapUnbeaten mapsWithRecords)
  unless (null unbeatenMaps) $ do
    void $ forkIO $ void do
      logInfo $ "Checking AT set by plugin for unbeaten maps: " <> displayShow (length unbeatenMaps)
      withAcid1 updateMaps
        =<< flip (pooledMapConcurrentlyN 2) (zip [(1::Int)..] unbeatenMaps) \(i, tmmap) -> do
          atSetByPlugin <- checkAtSetByPlugin (unTMXId $ _tmm_tmxId tmmap)
          logInfo $ "Checking AT set by plugin for map #" <> displayShow (unTMXId $ _tmm_tmxId tmmap) <> "(" <> displayShow i <> "/" <> displayShow (length mapsWithUserInfo) <> "): " <> displayShow atSetByPlugin
          return $ (defPatch $ _tmm_tmxId tmmap){_tmmp_atSetByPlugin = Just atSetByPlugin}

  refreshBeatenMaps

addMissingMaps :: (MonadReader env m, HasTMXClient env, HasState env, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env) => [Int] -> m ()
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

refreshMissingInfo :: (MonadIO m, MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasAppSettings env, HasNadeoThrottler env, HasNadeoRequestRate env, HasLogFunc env) => m ()
refreshMissingInfo = do
  maps <- filter (isNothing . _tmm_authorUid) <$> queryAcid GetMaps
  logInfo $ "Reloading missing info for maps: " <> displayShow (length maps)
  void $ withAcid1 updateMaps =<< loadNadeoMapsInfo maps

refreshRecentUnbeatenMaps :: (MonadIO m, MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasAppSettings env, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env) => m ()
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

refreshUnbeatenMaps :: (MonadIO m, MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasAppSettings env, HasNadeoRequestRate env, HasNadeoThrottler env, HasLogFunc env) => m ()
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
        logInfo $ displayShow (_tmm_tmxId tmmap) <> ": got nb players for map " <> displayShow (_tmm_tmxId tmmap) <> ": " <> displayShow (_xmpr_nb_players res)
        updateAcid (ReplaceMap tmmap{_tmm_nbPlayers = Just (_xmpr_nb_players res)})

rescanMaps :: (MonadReader env m, HasState env, MonadUnliftIO m, HasAppSettings env, HasLogFunc env) => m ()
rescanMaps = do
  maps <- filter (isNothing . _tmm_atSetByPlugin) <$> queryAcid GetMaps
  forM_ (zip [(1 :: Int) ..] maps) $ \(i, tmmap) -> do
    logInfo $ "Checking map #" <> displayShow i <> "/" <> displayShow (length maps) <> ": " <> displayShow (_tmm_tmxId tmmap)
    case tmmap of
      TMMap{_tmm_atSetByPlugin = Nothing, _tmm_tmxId = tmxId} -> do
        atSetByPlugin <- checkAtSetByPlugin $ unTMXId tmxId
        logInfo $ "Checking AT set by plugin for map " <> displayShow tmxId <> ": " <> displayShow atSetByPlugin
        if isJust atSetByPlugin
          then updateAcid (SetAtSetByPlugin tmxId atSetByPlugin)
          else pass
      _ -> pass
