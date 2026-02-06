{-# LANGUAGE TemplateHaskell #-}

module MapMonitor.API.Nadeo (
  GetMapLeaderboardTopEntry (..),
  HasNadeoThrottler (..),
  GetMapLeaderboardTop (..),
  GetMapLeaderboardResponse (..),
  UTCTimestamp (..),
  GetMapMultipleResponseMap (..),
  GetMapMultipleResponse (..),
  IdsList (..),
  HasNadeoTokenState (..),
  HasNadeoCoreClient (..),
  HasNadeoLiveClient (..),
  HasNadeoAuthToken (..),
  NadeoTokenState (..),
  nadeoGetMapLeaderboard,
  nadeoGetMapMultiple,
  NadeoAuthTokenResponse (..),
  NadeoBasicAuthRequestBody (..),
  HasNadeoRequestRate(..),
  authorizeNadeo,
)
where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import RIO.Time
import RIO (logError, HasLogFunc, displayShow)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Protolude hiding (threadDelay, atomically)
import qualified RIO.Text as Text
import Servant.API
import Servant.Client
import UnliftIO.STM
import UnliftIO.Concurrent
import MapMonitor.API.Util
import Data.Fixed (Pico)

data NadeoTokenState
  = NadeoTokenState
  { _nadeoTokenState_accessToken :: !Text
  , _nadeoTokenState_accessExpires :: !UTCTime
  , _nadeoTokenState_refreshToken :: !Text
  , _nadeoTokenState_refreshExpires :: !UTCTime
  }

class HasNadeoRequestRate env where
  nadeoRequestRateL :: Lens' env Pico

class HasNadeoThrottler env where
  nadeoThrottlerL :: Lens' env (TMVar UTCTime)

withThrottler :: (MonadIO m, MonadReader env m, HasNadeoThrottler env, HasNadeoRequestRate env) => m a -> m a
withThrottler action = do
  throttler <- view nadeoThrottlerL
  nextRequestAllowedAt <- atomically $ takeTMVar throttler
  do
    now <- getCurrentTime
    when (now < nextRequestAllowedAt) do
      threadDelay (floor $ (nominalDiffTimeToSeconds $ nextRequestAllowedAt `diffUTCTime` now) * 1000000)

  res <- action

  do
    rate <- view nadeoRequestRateL
    now <- getCurrentTime
    atomically $ putTMVar throttler $ addUTCTime (secondsToNominalDiffTime $ rate) now

  return res

class HasNadeoTokenState env where
  nadeoTokenStateL :: Lens' env (TMVar (Maybe NadeoTokenState))

class HasNadeoCoreClient env where
  nadeoCoreClientL :: Lens' env ClientEnv

class HasNadeoLiveClient env where
  nadeoLiveClientL :: Lens' env ClientEnv

class HasNadeoAuthToken env where
  nadeoAuthTokenL :: Lens' env Text

data NadeoAuthTokenResponse
  = NadeoAuthTokenResponse
  { _nadtr_accessToken :: !Text
  , _nadtr_refreshToken :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_nadtr_")} ''NadeoAuthTokenResponse)

type NadeoBasicAuth = Header' '[Required] "Authorization" Text

data NadeoBasicAuthRequestBody
  = NadeoBasicAuthRequestBody
  { _nbarb_audience :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_nbarb_")} ''NadeoBasicAuthRequestBody)

data NadeoTokenHeader
  = NadeoTokenHeader
  { _nadth_token :: !Text
  }
  deriving (Show)

instance ToHttpApiData NadeoTokenHeader where
  toUrlPiece tok = "nadeo_v1 t=" <> _nadth_token tok

type NadeoTokenAuth = Header' '[Required] "Authorization" NadeoTokenHeader

type CoreNadeoAPI =
  "v2"
    :> ( "authentication"
           :> "token"
           :> ( "basic" :> NadeoBasicAuth :> ReqBody '[JSON] NadeoBasicAuthRequestBody :> Post '[JSON] NadeoAuthTokenResponse
                  :<|> "refresh" :> NadeoTokenAuth :> Post '[JSON] NadeoAuthTokenResponse
              )
       )

coreNadeoAPI :: Proxy CoreNadeoAPI
coreNadeoAPI = Proxy

authorizeNadeo' :<|> refreshTokenNadeo' = client coreNadeoAPI

authorizeNadeo :: (MonadIO m, MonadReader env m, HasNadeoCoreClient env, HasNadeoThrottler env, HasNadeoRequestRate env)
               => Text
               -> Text
               -> m (Either ClientError NadeoAuthTokenResponse)
authorizeNadeo auth scope = do
  withThrottler do
    runInClient nadeoCoreClientL $ authorizeNadeo' ("Basic " <> auth) (NadeoBasicAuthRequestBody scope)


data GetMapLeaderboardTopEntry
  = GetMapLeaderboardTopEntry
  { _gmlte_accountId :: !Text
  , _gmlte_position :: !Int
  , _gmlte_score :: !Int
  , _gmlte_timestamp :: !Int
  , _gmlte_zoneId :: !Text
  , _gmlte_zoneName :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmlte_")} ''GetMapLeaderboardTopEntry)

data GetMapLeaderboardTop
  = GetMapLeaderboardTop
  { _gmlt_top :: ![GetMapLeaderboardTopEntry]
  , _gmlt_zoneId :: !Text
  , _gmlt_zoneName :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmlt_")} ''GetMapLeaderboardTop)

data GetMapLeaderboardResponse
  = GetMapLeaderboardResponse
  { _gmlr_groupUid :: !Text
  , _gmlr_mapUid :: !Text
  , _gmlr_tops :: ![GetMapLeaderboardTop]
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmlr_")} ''GetMapLeaderboardResponse)

newtype UTCTimestamp
  = UTCTimestamp {unUTCTimestamp :: UTCTime}
  deriving (Show)

instance FromJSON UTCTimestamp where
  parseJSON v =
    parseJSON @Int v
      >>= \i -> return $ UTCTimestamp (posixSecondsToUTCTime (fromIntegral i))

data GetMapMultipleResponseMap
  = GetMapMultipleResponseMap
  { _gmmrm_author :: !Text
  , _gmmrm_uid :: !Text
  , _gmmrm_uploadTimestamp :: !UTCTimestamp
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmmrm_")} ''GetMapMultipleResponseMap)

data GetMapMultipleResponse
  = GetMapMultipleResponse
  { _gmmr_mapList :: ![GetMapMultipleResponseMap]
  , _gmmr_itemCount :: !Int
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmmr_")} ''GetMapMultipleResponse)

data IdsList
  = IdsList [Text]
  deriving (Show)

instance ToHttpApiData IdsList where
  toUrlPiece (IdsList ids) = Text.intercalate "," ids

type LiveServicesNadeoAPI =
  "api"
    :> "token"
    :> ( "leaderboard"
           :> "group"
           :> "Personal_Best"
           :> "map"
           :> Capture "mapId" Text
           :> "top"
           :> NadeoTokenAuth
           :> QueryParam' '[Required] "length" Int
           :> QueryParam' '[Required] "onlyWorld" Text
           :> Get '[JSON] GetMapLeaderboardResponse
           :<|> "map"
             :> "get-multiple"
             :> NadeoTokenAuth
             :> QueryParam' '[Required] "mapUidList" IdsList
             :> Get '[JSON] GetMapMultipleResponse
       )

withAccessToken :: (MonadFail m, MonadIO m, HasNadeoTokenState env, MonadReader env m, HasNadeoAuthToken env, HasNadeoCoreClient env, HasNadeoThrottler env, HasNadeoRequestRate env, HasLogFunc env) => (NadeoTokenHeader -> m a) -> m a
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
    nadeoAuth <- view nadeoAuthTokenL
    accessTokenE <- authorizeNadeo nadeoAuth "NadeoLiveServices"
    case accessTokenE of
      Left err -> do
        logError $ "Error: " <> displayShow err
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

liveServicesNadeoAPI :: Proxy LiveServicesNadeoAPI
liveServicesNadeoAPI = Proxy

nadeoGetMapLeaderboard' :<|> nadeoGetMapMultiple' = client liveServicesNadeoAPI

nadeoGetMapLeaderboard :: (MonadIO m, MonadReader s m, HasNadeoThrottler s,  HasNadeoRequestRate s, HasNadeoLiveClient s, MonadFail m, HasNadeoTokenState s, HasNadeoAuthToken s, HasNadeoCoreClient s, HasLogFunc s) => Text -> Int -> m (Either ClientError GetMapLeaderboardResponse)
nadeoGetMapLeaderboard mapUid count =
  withAccessToken \accessToken -> do
    withThrottler do
      runInClient nadeoLiveClientL $ nadeoGetMapLeaderboard' mapUid accessToken count "true"

nadeoGetMapMultiple :: (MonadIO m, MonadReader s m, HasNadeoThrottler s,  HasNadeoRequestRate s, HasNadeoLiveClient s, MonadFail m, HasNadeoTokenState s, HasNadeoAuthToken s, HasNadeoCoreClient s, HasLogFunc s) => IdsList -> m (Either ClientError GetMapMultipleResponse)
nadeoGetMapMultiple ids =
  withAccessToken \accessToken -> do
    withThrottler do
      runInClient nadeoLiveClientL $ nadeoGetMapMultiple' accessToken ids
