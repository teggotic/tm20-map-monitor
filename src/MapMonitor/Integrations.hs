module MapMonitor.Integrations
where

import Control.Lens
import UnliftIO.Retry
import Data.List (last)
import qualified Data.Set as Set
import GHC.Exts
import MapMonitor.API.Nadeo
import MapMonitor.API.TMX
import MapMonitor.API.XertroV
import MapMonitor.ATCheck (checkAtSetByPlugin)
import MapMonitor.Common
import MapMonitor.DB
import Protolude hiding (atomically, threadDelay)
import qualified RIO.Map as Map
import RIO.Time
import Servant.Client
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import UnliftIO
import UnliftIO.Concurrent
import Conduit

iterTmxMaps :: (MonadIO m, MonadReader env m, HasTMXClient env) => Int -> Maybe Int -> ConduitT () TMMap m ()
iterTmxMaps cnt after = do
  respE <- runInClient tmxClientL $ tmxSearchMaps $ TMXSearchMaps{_tmxsm_ids = [], _tmxsm_count = Just (cnt `min` 200), _tmxsm_after = after, _tmxsm_from = Nothing, _tmxsm_order1 = Just 6}

  case respE of
    Left err -> do
      putText $ "Error: " <> show err
    Right resp -> do
      yieldMany $ tmxMapToTMMap <$> _tmxsr_Results resp
      when (_tmxsr_More resp) $ do
        iterTmxMaps cnt (Just $ _tmxsm_MapId $ last $ _tmxsr_Results resp)

recheckTmxForLatestMissingMaps :: (MonadReader env m, HasState env, HasTMXClient env,  HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env,  HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m) => Int -> m ()
recheckTmxForLatestMissingMaps cnt = do
  ids <- getAllKnownIds
  maps <- runConduit $ iterTmxMaps cnt Nothing .| takeC cnt .|  filterC (\tmmap -> not $ unTMXId (_tmm_tmxId tmmap) `elem` ids) .| sinkList
  collectUnknownMaps maps

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

loadNadeoMapsInfo :: (MonadIO m, HasNadeoLiveClient env, MonadReader env m, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env) => [TMMap] -> m [TMMapPatch]
loadNadeoMapsInfo tmmaps = do
  let uidDict = fromList $ fmap (\tmmap -> (_tmm_uid tmmap, defPatch $ _tmm_tmxId tmmap)) tmmaps
  updates <- forM (chunked 100 tmmaps) $ \maps -> do
    res <- withAccessToken \token ->
      runInClient nadeoLiveClientL $ nadeoGetMapMultiple token (IdsList $ fmap _tmm_uid maps)
    threadDelay (1000 * 1000)
    case res of
      Left err -> do
        putText $ "Error: " <> show err
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

refreshRecords :: (MonadIO m, MonadReader env m, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env) => [TMMap] -> m [TMMapPatch]
refreshRecords maps = do
  start <- liftIO $ getTime Monotonic
  putText $ "Refreshing records: " <> show start
  forM (zip [(1 :: Int) ..] maps) \(i, tmmap) -> do
    let mapPatch = defPatch $ _tmm_tmxId tmmap
    now <- liftIO $ getTime Monotonic
    let diffNs = toNanoSecs (diffTimeSpec now start)
        diffSec = fromIntegral diffNs / 1e9 :: Double
    putText $ "Pulling records for map #" <> show i <> "/" <> show (length maps) <> ": " <> show (_tmm_uid tmmap) <> " (" <> show diffSec <> "s)"
    records <- withAccessToken \accessToken -> do
      runInClient nadeoLiveClientL $ nadeoGetMapLeaderboard (_tmm_uid tmmap) accessToken 1 "true"
    updatedMap <- case records of
      Left err -> do
        putText $ "Error: " <> show err
        return mapPatch
      Right (GetMapLeaderboardResponse{_gmlr_tops = [GetMapLeaderboardTop{_gmlt_top = [topRecord]}]}) -> do
        putText $ "Got record for map " <> show (_tmm_uid tmmap)
        return $ mapPatch{_tmmp_currentWR = Just $ Just (TMMapRecord (_gmlte_accountId topRecord) (_gmlte_score topRecord) (_gmlte_timestamp topRecord))}
      err@_ -> do
        putText $ "No records found for map " <> show (_tmm_uid tmmap) <> " with error: " <> show err
        return mapPatch
    threadDelay (500 * 1000)
    return updatedMap

withAccessToken :: (MonadFail m, MonadIO m, HasNadeoTokenState env, MonadReader env m, HasAppSettings env, HasNadeoCoreClient env) => (NadeoTokenHeader -> m a) -> m a
withAccessToken m = do
  tokenStateRef <- view nadeoTokenStateL
  tokenStateM <- atomically $ takeTMVar tokenStateRef
  tokState <- case tokenStateM of
    Nothing -> do
      Just tok <- newToken
      return tok
    Just tokenState -> do
      now <- getCurrentTime
      if _nadeoTokenState_accessExpires tokenState > (addUTCTime (secondsToNominalDiffTime 60) now)
        then return tokenState
        else do
          Just tok <- newToken
          return tok
  atomically $ putTMVar tokenStateRef (Just tokState)
  m (NadeoTokenHeader $ _nadeoTokenState_accessToken tokState)
 where
  newToken = do
    settings <- view appSettingsL
    accessTokenE <- runInClient nadeoCoreClientL $ authorizeNadeo ("Basic " <> _settings_auth settings) (NadeoBasicAuthRequestBody "NadeoLiveServices")
    case accessTokenE of
      Left err -> do
        putText $ "Error: " <> show err
        return Nothing
      Right resp -> do
        now <- getCurrentTime
        return $
          Just $
            NadeoTokenState
              { _nadeoTokenState_accessToken = _nadtr_accessToken resp
              , _nadeoTokenState_accessExpires = addUTCTime (secondsToNominalDiffTime $ 60 * 60) now
              , _nadeoTokenState_refreshToken = _nadtr_refreshToken resp
              , _nadeoTokenState_refreshExpires = addUTCTime (secondsToNominalDiffTime $ 24 * 60 * 60) now
              }

refreshBeatenMaps :: (MonadIO m, MonadReader env m, HasState env) => m ()
refreshBeatenMaps = do
  mapsBeforeShuffle <- length <$> queryAcid GetMaps
  updateAcid ShuffleBeatenMaps
  mapsAfterShuffle <- length <$> queryAcid GetMaps
  putText $ show (mapsBeforeShuffle - mapsAfterShuffle) <> " maps were beaten"

getAllKnownIds :: (MonadReader env m, MonadIO m, HasState env) => m (Set Int)
getAllKnownIds = do
  st <- queryAcid GetMapMonitorState
  return $ fromList $ fmap (unTMXId . _tmm_tmxId) $ (Map.elems $ mms_ubeatenMaps st) <> mms_beatenMaps st

collectUnknownMaps :: (MonadReader env m, HasState env, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m) => [TMMap] -> m ()
collectUnknownMaps [] = pass
collectUnknownMaps newMaps = do
  updateAcid (AddNewMaps (zip (_tmm_tmxId <$> newMaps) newMaps))

  mapsWithUserInfo <- withAcid1 updateMaps =<< loadNadeoMapsInfo newMaps

  let
    doRefreshRecords =
      void $ withAcid1 updateMaps =<< refreshRecords (filter (isJust . _tmm_authorUid) mapsWithUserInfo)

    doCheckAtSetByPlugin =
      void $
        withAcid1 updateMaps
          =<< flip (pooledMapConcurrentlyN 4) (zip [(1::Int)..] mapsWithUserInfo) \(i, tmmap) -> do
            atSetByPlugin <- checkAtSetByPlugin (unTMXId $ _tmm_tmxId tmmap)
            putText $ "Checking AT set by plugin for map #" <> show (unTMXId $ _tmm_tmxId tmmap) <> "(" <> show i <> "/" <> show (length mapsWithUserInfo) <> "): " <> show atSetByPlugin
            return $ (defPatch $ _tmm_tmxId tmmap){_tmmp_atSetByPlugin = Just atSetByPlugin}

  concurrently_ doRefreshRecords doCheckAtSetByPlugin

  refreshBeatenMaps

addMissingMaps :: (MonadReader env m, HasTMXClient env, HasState env, HasNadeoLiveClient env, MonadFail m, HasNadeoTokenState env, HasAppSettings env, HasNadeoCoreClient env, MonadUnliftIO m) => [Int] -> m ()
addMissingMaps [] = pass
addMissingMaps ids = do
  knownIds <- getAllKnownIds
  let newMaps = fromList ids `Set.difference` knownIds
  if null newMaps
    then pass
    else do
      resE <- runInClient tmxClientL $ pullKnownMaps ids
      case resE of
        Left err -> putText $ "Error: " <> show err
        Right res -> do
          putText $ "Got " <> show (length res) <> " maps"
          let maps = tmxMapToTMMap <$> res
          print maps

          collectUnknownMaps maps

refreshMissingInfo :: (MonadIO m, MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasAppSettings env) => m ()
refreshMissingInfo = do
  maps <- filter (isNothing . _tmm_authorUid) <$> queryAcid GetMaps
  putText $ "Reloading missing info for maps: " <> show (length maps)
  void $ withAcid1 updateMaps =<< loadNadeoMapsInfo maps

refreshRecentUnbeatenMaps :: (MonadIO m, MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasAppSettings env) => m ()
refreshRecentUnbeatenMaps = do
  putText "Refreshing records on recently uploaded unbeaten maps"
  now <- getCurrentTime
  let last7Days = addUTCTime (negate $ secondsToNominalDiffTime $ 60 * 60 * 24 * 7) now
  let uploadedRecently (_tmm_uploadedAt -> Just uploadedAt) = uploadedAt > last7Days
      uploadedRecently _ = False

  maps <- filter (liftA2 (&&) (isJust . _tmm_authorUid) uploadedRecently) <$> queryAcid GetMaps
  putText $ "Unbeaten maps uploaded recently: " <> show (length maps)
  void $ withAcid1 updateMaps =<< refreshRecords maps

  refreshBeatenMaps

refreshUnbeatenMaps :: (MonadIO m, MonadFail m, MonadReader env m, HasState env, HasNadeoTokenState env, HasNadeoCoreClient env, HasNadeoLiveClient env, HasAppSettings env) => m ()
refreshUnbeatenMaps = do
  putText "Refreshing records on all unbeaten maps"
  maps <- filter (isJust . _tmm_authorUid) <$> queryAcid GetMaps

  void $ withAcid1 updateMaps =<< refreshRecords maps

  refreshBeatenMaps

refreshNbPlayers :: (MonadReader env m, HasState env, HasXertrovClient env, MonadUnliftIO m) => m ()
refreshNbPlayers = do
  maps <- filter (isJust . _tmm_authorUid) <$> queryAcid GetMaps
  flip (pooledMapConcurrentlyN_ 4) maps \tmmap -> do
    runInClient xertrovClientL (xertrovGetNbPlayers (_tmm_uid tmmap)) >>= \case
      Left err -> do
        putText $ show (_tmm_tmxId tmmap) <> ": error: " <> show err
      Right res -> do
        putText $ show (_tmm_tmxId tmmap) <> ": got nb players for map " <> show (_tmm_tmxId tmmap) <> ": " <> show (_xmpr_nb_players res)
        updateAcid (ReplaceMap tmmap{_tmm_nbPlayers = Just (_xmpr_nb_players res)})

rescanMaps :: (MonadReader env m, HasState env, MonadUnliftIO m) => m ()
rescanMaps = do
  maps <- filter (isNothing . _tmm_atSetByPlugin) <$> queryAcid GetMaps
  forM_ (zip [(1 :: Int) ..] maps) $ \(i, tmmap) -> do
    putText $ "Checking map #" <> show i <> "/" <> show (length maps) <> ": " <> show (_tmm_tmxId tmmap)
    case tmmap of
      TMMap{_tmm_atSetByPlugin = Nothing, _tmm_tmxId = tmxId} -> do
        atSetByPlugin <- checkAtSetByPlugin $ unTMXId tmxId
        putText $ "Checking AT set by plugin for map " <> show tmxId <> ": " <> show atSetByPlugin
        if isJust atSetByPlugin
          then updateAcid (SetAtSetByPlugin tmxId atSetByPlugin)
          else pass
      _ -> pass
