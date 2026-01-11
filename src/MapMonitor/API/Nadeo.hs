{-# LANGUAGE TemplateHaskell #-}

module MapMonitor.API.Nadeo (
  GetMapLeaderboardTopEntry (..),
  GetMapLeaderboardTop (..),
  GetMapLeaderboardResponse (..),
  UTCTimestamp (..),
  GetMapMultipleResponseMap (..),
  GetMapMultipleResponse (..),
  IdsList (..),
  NadeoTokenHeader (..),
  nadeoGetMapLeaderboard,
  nadeoGetMapMultiple,
  NadeoAuthTokenResponse (..),
  NadeoBasicAuthRequestBody (..),
  authorizeNadeo,
  refreshTokenNadeo,
)
where

import Data.Aeson
import Data.Aeson.TH
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Protolude
import RIO.Prelude.Types
import qualified RIO.Text as Text
import Servant.API
import Servant.Client

data GetMapLeaderboardTopEntry
  = GetMapLeaderboardTopEntry
  { _gmlte_accountId :: Text
  , _gmlte_position :: Int
  , _gmlte_score :: Int
  , _gmlte_timestamp :: Int
  , _gmlte_zoneId :: Text
  , _gmlte_zoneName :: Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmlte_")} ''GetMapLeaderboardTopEntry)

data GetMapLeaderboardTop
  = GetMapLeaderboardTop
  { _gmlt_top :: [GetMapLeaderboardTopEntry]
  , _gmlt_zoneId :: Text
  , _gmlt_zoneName :: Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmlt_")} ''GetMapLeaderboardTop)

data GetMapLeaderboardResponse
  = GetMapLeaderboardResponse
  { _gmlr_groupUid :: Text
  , _gmlr_mapUid :: Text
  , _gmlr_tops :: [GetMapLeaderboardTop]
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
  { _gmmrm_author :: Text
  , _gmmrm_uid :: Text
  , _gmmrm_uploadTimestamp :: UTCTimestamp
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmmrm_")} ''GetMapMultipleResponseMap)

data GetMapMultipleResponse
  = GetMapMultipleResponse
  { _gmmr_mapList :: [GetMapMultipleResponseMap]
  , _gmmr_itemCount :: Int
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gmmr_")} ''GetMapMultipleResponse)

data IdsList
  = IdsList [Text]
  deriving (Show)

instance ToHttpApiData IdsList where
  toUrlPiece (IdsList ids) = Text.intercalate "," ids

data NadeoTokenHeader
  = NadeoTokenHeader
  { _nadth_token :: Text
  }
  deriving (Show)

instance ToHttpApiData NadeoTokenHeader where
  toUrlPiece tok = "nadeo_v1 t=" <> _nadth_token tok

type NadeoTokenAuth = Header' '[Required] "Authorization" NadeoTokenHeader

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

liveServicesNadeoAPI :: Proxy LiveServicesNadeoAPI
liveServicesNadeoAPI = Proxy

nadeoGetMapLeaderboard :: Text -> NadeoTokenHeader -> Int -> Text -> ClientM GetMapLeaderboardResponse
nadeoGetMapMultiple :: NadeoTokenHeader -> IdsList -> ClientM GetMapMultipleResponse
nadeoGetMapLeaderboard :<|> nadeoGetMapMultiple = client liveServicesNadeoAPI

data NadeoAuthTokenResponse
  = NadeoAuthTokenResponse
  { _nadtr_accessToken :: Text
  , _nadtr_refreshToken :: Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_nadtr_")} ''NadeoAuthTokenResponse)

type NadeoBasicAuth = Header' '[Required] "Authorization" Text

data NadeoBasicAuthRequestBody
  = NadeoBasicAuthRequestBody
  { _nbarb_audience :: Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_nbarb_")} ''NadeoBasicAuthRequestBody)

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

authorizeNadeo :<|> refreshTokenNadeo = client coreNadeoAPI
