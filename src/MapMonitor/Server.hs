{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module MapMonitor.Server
where

import qualified Data.UUID.V4 as UUID4
import Servant.Multipart as MP
import Data.IxSet.Typed
import qualified System.ZMQ4 as ZMQ
import Control.Category (id)
import Control.Lens hiding ((.=), (<.>))
import Data.Acid
import qualified Data.Map as Map
import MapMonitor.API
import MapMonitor.API.OpenPlanet
import MapMonitor.CachedAPIResponses
import MapMonitor.Common
import MapMonitor.DB
import MapMonitor.Integrations
import qualified Network.HTTP.Types as H
import Network.Wai as Wai
import PingRPC
import Protolude hiding (finally, threadDelay, (<.>), wait, withAsync, atomically)
import RIO (toStrictBytes, HasLogFunc (..), LogFunc, finally)
import qualified RIO.Text as Text
import RIO.Time
import Servant
import Servant.Auth.Server
import MapMonitor.ReplayValidation
import Servant.Client
import UnliftIO.Concurrent
import UnliftIO.Async
import Servant.Server
import UnliftIO.STM
import MapMonitor.API.Nadeo
import MapMonitor.API.XertroV
import MapMonitor.API.TMX
import MapMonitor.API.Util
import Data.Fixed
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.Minio
import System.FilePath ((</>), (<.>))
import System.Process.Typed
import qualified RIO.Text as T
import Data.Aeson (encodeFile, object, (.=), decode)
import Data.Aeson.Types
import Data.Aeson.Key (fromString)
import RIO.FilePath (takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import Data.Aeson.TH (deriveFromJSON)
import UnliftIO.Directory (removeFile)

data AppState
  = AppState
  { _appState_acid :: !(AcidState MapMonitorState)
  , _appState_unbeatenAtsCache :: !(TVar UnbeatenAtsResponse)
  , _appState_beatenAtsCache :: !(TVar RecentlyBeatenAtsResponse)
  , _appState_coreNadeoClient :: !ClientEnv
  , _appState_liveServicesNadeoClient :: !ClientEnv
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
  }

$(makeLenses ''AppState)

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

instance HasUnbeatenAtsCache AppState where
  unbeatenAtsCacheL = appState_unbeatenAtsCache

instance HasBeatenAtsCache AppState where
  beatenAtsCacheL = appState_beatenAtsCache

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

type AppM = ReaderT AppState Servant.Server.Handler

mapMonitorAPI :: Proxy MapMonitorAPI
mapMonitorAPI = Proxy
redirectTo :: Text -> Wai.Response
redirectTo url =
  responseLBS
    H.status301
    [(H.hContentType, "text/plain"), (H.hLocation, encodeUtf8 url)]
    "Redirect"

downloadMapsServer :: ServerT DownloadMapAPI AppM
downloadMapsServer = downloadMap
 where
  downloadMap :: Int -> ServerT Raw AppM
  downloadMap mapId = Tagged $ \_ resp -> do
    resp $ redirectTo $ "https://trackmania.exchange/maps/download/" <> show mapId

tmxApiServer :: ServerT TMXApi AppM
tmxApiServer = unbeaten :<|> unbeatenLeaderboard :<|> beaten
 where
  beaten = do
    readTVarIO =<< view beatenAtsCacheL

  unbeatenLeaderboard = do
    return $
      UnbeatenAtsLeaderboardResponse
        { _ualr_count_to_pos = mempty
        , _ualr_players = [("c331bdbf-2182-4a51-813d-87d6f0f209c5", 1)]
        , _ualr_nb_players = 1
        , _ualr__info = "{ count_to_pos: {[score]: (rank, nb_eq_players)}, players: [(user, score)] }"
        }

  unbeaten = do
    readTVarIO =<< view unbeatenAtsCacheL

managementApiServer :: AuthResult AUser -> ServerT ManagementAPI AppM
managementApiServer (Authenticated auser) = managementReportMap :<|> managementDeleteReport :<|> managementAddMissingMap
 where
  managementReportMap tmxId payload = do
    -- putText $ "Reporting map: " <> show tmxId <> " with payload: " <> show payload
    now <- getCurrentTime
    withAcid2 reportMap (TMXId tmxId) (_auser_uid auser, now, _rmp_reason payload)
    refreshCaches
    return NoContent

  managementDeleteReport tmxId = do
    void $ withAcid1 updateMaps $ [(defPatch $ TMXId tmxId) {_tmmp_reportedBy = Just $ Map.fromList [(_auser_uid auser, Nothing)]}]
    refreshCaches
    return NoContent

  managementAddMissingMap tmxId = do
    runAppState do
      addMissingMaps [tmxId]
    refreshCaches
    return NoContent
managementApiServer _ = throwAll err404

trustedUsers :: [Text]
trustedUsers = ["c331bdbf-2182-4a51-813d-87d6f0f209c5", "65ce1935-d166-42b3-89a6-6345ccf41865", "59b84907-59fb-4455-b31d-b0cc44c36ec7", "bce4d579-dc66-43b5-9d57-eb1fb58dd450", "296a77c2-1c19-4236-9a3e-28c8c01e6312"]

authApiServer :: ServerT AuthAPI AppM
authApiServer = authOpenplanetToken :<|> authIsTrusted
 where
  authOpenplanetToken tok = do
    openplanetSecret <- _settings_openplanetAuthSecret <$> view appSettingsL
    runInClient openPlanetClientL (openPlanetAuthValidate $ OpenPlanetAuthValidateRequest{_opavr_token = (_ia_token tok), _opavr_secret = openplanetSecret})
      >>= \case
        Left err -> do
          print err
          throwError err401
        Right (OpenPlanetAuthValidateResponse{_opavr_account_id = accountId}) -> do
          if accountId `elem` trustedUsers
            then do
              ajwt <- asks _appState_jwtSettings
              now <- getCurrentTime
              Right newTokBs <- liftIO $ makeJWT (AUser accountId) ajwt (Just $ (secondsToNominalDiffTime $ 60 * 60 * 24) `addUTCTime` now)
              return $ InternalAuth{_ia_token = decodeUtf8 $ toStrictBytes newTokBs}
            else throwAll err401

  authIsTrusted accountId = do
    return $ accountId `elem` trustedUsers

runAppState :: (MonadReader AppState m, MonadIO m) => ReaderT AppState IO a -> m a
runAppState m = do
  st <- ask
  liftIO $ runReaderT m st

-- {"/tmp/map-monitor-validation-b6aa46134a327852/447099bd-0928-4996-8cc2-2f4c0d8b535c.Replay.Gbx":{"ValidatedResult":{"NbCheckpoints":1,"NbRespawns":0,"Time":7786,"Score":0},"IsValid":true,"Desc":null,"FileName":"a4f31d82-ed96-4001-8756-881ed8f6be95.Ghost.Gbx","MapUid":"OKbCJHQrD7IuINQPTJi0lPf8w1h"}} 

server1 :: ServerT MapMonitorAPI AppM
server1 = tmxApiServer :<|> downloadMapsServer :<|> managementApiServer :<|> authApiServer :<|> staticServer :<|> uploadFile
  where
    staticServer = serveDirectoryWebApp "/home/teggot/projects/haskell/map-monitor/assets"

    uploadFile multipartData = do
      runAppState do
        flip finally (removeFile $ _vru_replay $ multipartData) $ do
          s <- flip withAsync wait $ do
            validateReplay multipartData >>= \case
              Left err -> do
                return $ "Validation failed: " <> show err
              Right _ -> do
                return $ "Validation successful"
          return $ "<div>" <> show (_vru_tmxid multipartData) <> ": " <> s <> "</div>"

fallbackApp :: Application
fallbackApp req respond = do
  respond $ responseLBS H.status404 [] ";...;"

app :: Servant.Server.Context '[CookieSettings, JWTSettings] -> AppState -> Application
app cfg state req respond = do
  -- putText $ "Request: " <> show req
  let servantApp =
        serveWithContext mapMonitorAPI cfg $
          hoistServerWithContext
            mapMonitorAPI
            (Proxy :: Proxy '[CookieSettings, JWTSettings])
            (`runReaderT` state)
            server1
  servantApp req $ \res ->
    if is404 res
      then fallbackApp req respond
      else respond res

is404 :: Wai.Response -> Bool
is404 res =
  case Wai.responseStatus res of
    s -> H.statusCode s == 404

collectBeatenAtsResponse :: (MonadIO m, MonadReader env m, HasState env) => m RecentlyBeatenAtsResponse
collectBeatenAtsResponse = do
  st <- queryAcid GetMapMonitorState
  let
    beatenMaps = (@= Beaten) (_mms_maps st)
    allMaps = take 200 $ toRow <$> toDescList (Proxy @WrTimestamp) beatenMaps
    below100k = take 200 $ toRow <$> toDescList (Proxy @WrTimestamp) (beatenMaps @< TMXId 100000)
  return $
    RecentlyBeatenAtsResponse
      { _rbar_keys = ["TrackID", "TrackUID", "Track_Name", "AuthorLogin", "Tags", "MapType", "AuthorTime", "WR", "LastChecked", "ATBeatenTimestamp", "ATBeatenUsers", "NbPlayers"]
      , _rbar_all = RecentlyBeatenAtsTracks (length allMaps) allMaps
      , _rbar_below100k = RecentlyBeatenAtsTracks (length below100k) below100k
      }
 where
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

collectUnbeatenAtsResponse :: (MonadIO m, MonadReader env m, HasState env) => m UnbeatenAtsResponse
collectUnbeatenAtsResponse = do
  allMaps <- queryAcid GetMaps
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
        , _uat_nbPlayers = fromMaybe 123456 (_tmm_nbPlayers tmmap)
        , _uat_isHidden = isJust (_tmm_hiddenReason tmmap)
        , _uat_reason = fromMaybe "" (_tmm_hiddenReason tmmap)
        , _uat_atSetByPlugin = fromMaybe False (_tmm_atSetByPlugin tmmap)
        , _uat_reported = (\(k, (_, r)) -> (k, r)) <$> Map.assocs (_tmm_reportedBy tmmap)
        , _uat_uploadedTimestamp = maybe 0 (nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) (_tmm_uploadedAt tmmap)
        , _uat_validation = if (_tmm_tmxId tmmap) == 285216
                              then (True, "https://map-monitor-test.nbg1.your-objectstorage.com/as7WsEWDwP4g56F4bJZQ4KYiIA9.Ghost.Gbx")
                              else if (_tmm_tmxId tmmap) == 285217
                                then (True, "")
                                else (False, "")
        }

  return $
    UnbeatenAtsResponse
      { _uar_keys = ["TrackID", "TrackUID", "Track_Name", "AuthorLogin", "Tags", "MapType", "AuthorTime", "WR", "LastChecked", "NbPlayers", "IsHidden", "Reason", "AtSetByPlugin", "Reported", "UploadedTimestamp", "Validation"]
      , _uar_tracks = unbeatenMaps
      , _uar_nbTracks = length unbeatenMaps
      }

refreshCaches :: (MonadIO m, MonadReader env m, HasState env, HasBeatenAtsCache env, HasUnbeatenAtsCache env) => m ()
refreshCaches = do
  view unbeatenAtsCacheL >>= \c ->
    atomically . writeTVar c =<< collectUnbeatenAtsResponse
  view beatenAtsCacheL >>= \c ->
    atomically . writeTVar c =<< collectBeatenAtsResponse
