{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module MapMonitor.API
where

import Data.Aeson
import Data.Aeson.TH
import MapMonitor.DB
import MapMonitor.ServantCache
import Network.HTTP.Media ((//), (/:))
import Protolude
import qualified RIO.ByteString as BS
import qualified RIO.Text as Text
import Servant.API
import Servant.Auth
import Servant.Auth.JWT
import Servant.Multipart
import MapMonitor.GridDB (Grid, PosixTS, ID)
import Data.Fixed (Pico)
import GHC.Exts
import RIO.Time (UTCTime)

data AUser = AUser
  { _auser_uid :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_auser_")} ''AUser)
instance FromJWT AUser
instance ToJWT AUser

data InternalAuth
  = InternalAuth
  { _ia_token :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_ia_")} ''InternalAuth)

data EnrichedGrid
  = EnrichedGrid
  { _eg_grid :: Grid
  , _eg_playersCount :: Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_eg_")} ''EnrichedGrid)

type DownloadMapAPI =
  "maps" :> (
    "download" :> Capture "mapId" Int :> Raw
      :<|> Capture "mapId" Int :> "thumbnail" :> Raw
      :<|> Capture "mapUid" Text :> "notify" :> Get '[JSON] NoContent)

data UnbeatenAtTrack
  = UnbeatenAtTrack
  { _uat_trackId :: !TMXId
  , _uat_trackUid :: !Text
  , _uat_trackName :: !Text
  , _uat_authorLogin :: !Text
  , _uat_tags :: !Text
  , _uat_mapType :: !Text
  , _uat_authorTime :: !Int
  , _uat_wr :: !Int
  , _uat_lastChecked :: !Double
  , _uat_nbPlayers :: !Int
  , _uat_isHidden :: !Bool
  , _uat_reason :: !Text
  , _uat_atSetByPlugin :: !Bool
  , _uat_reported :: ![(Text, Text)]
  , _uat_uploadedTimestamp :: !Pico
  , _uat_validation :: !(Bool, Text)
  , _uat_fileSize :: !Int
  , _uat_info :: [TMInfo]
  }
  deriving (Show, Generic)

instance ToJSON UnbeatenAtTrack where
  toJSON (UnbeatenAtTrack{..}) =
    Array $
      fromList
        [ toJSON _uat_trackId
        , toJSON _uat_trackUid
        , toJSON _uat_trackName
        , toJSON _uat_authorLogin
        , toJSON _uat_tags
        , toJSON _uat_mapType
        , toJSON _uat_authorTime
        , toJSON _uat_wr
        , toJSON _uat_lastChecked
        , toJSON _uat_nbPlayers
        , toJSON _uat_isHidden
        , toJSON _uat_reason
        , toJSON _uat_atSetByPlugin
        , toJSON _uat_reported
        , toJSON _uat_uploadedTimestamp
        , toJSON _uat_validation
        , toJSON _uat_fileSize
        , toJSON _uat_info
        ]

data UnbeatenAtsResponse t
  = UnbeatenAtsResponse
  { _uar_keys :: ![Text]
  , _uar_tracks :: ![t]
  , _uar_nbTracks :: !Int
  }
  deriving (Show)

$(deriveToJSON defaultOptions{fieldLabelModifier = drop (length @[] "_uar_")} ''UnbeatenAtsResponse)

type BeatenAtTrack = (TMXId, Text, Text, Text, Text, Text, Int, Int, Int, Int, Text, Int)

data RecentlyBeatenAtsTracks
  = RecentlyBeatenAtsTracks
  { _rbtr_nbTracks :: !Int
  , _rbtr_tracks :: ![BeatenAtTrack]
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length @[] "_rbtr_")} ''RecentlyBeatenAtsTracks)

data RecentlyBeatenAtsResponse
  = RecentlyBeatenAtsResponse
  { _rbar_keys :: ![Text]
  , _rbar_all :: !RecentlyBeatenAtsTracks
  , _rbar_below100k :: !RecentlyBeatenAtsTracks
  , _rbar_below200k :: !RecentlyBeatenAtsTracks
  , _rbar_below300k :: !RecentlyBeatenAtsTracks
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length @[] "_rbar_")} ''RecentlyBeatenAtsResponse)

data UnbeatenAtsLeaderboardResponse
  = UnbeatenAtsLeaderboardResponse
  { _ualr_count_to_pos :: !(Map Int (Int, Int))
  , _ualr_players :: !([(Text, Int)])
  , _ualr_nb_players :: !Int
  , _ualr__info :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length @[] "_ualr_")} ''UnbeatenAtsLeaderboardResponse)

data MapInfo
  = MapInfo
  { _mi_trackId :: !TMXId
  , _mi_trackUid :: !Text
  , _mi_trackName :: !Text
  , _mi_authorLogin :: !Text
  , _mi_tags :: !Text
  , _mi_mapType :: !Text
  , _mi_authorTime :: !Int
  , _mi_wr :: !Int
  , _mi_nbPlayers :: !Int
  , _mi_isHidden :: !Bool
  , _mi_reason :: !Text
  , _mi_atSetByPlugin :: !Bool
  , _mi_reported :: ![(Text, Text)]
  , _mi_uploadedTimestamp :: !Pico
  , _mi_validation :: !(Bool, Text)
  , _mi_fileSize :: !Int
  , _mi_info :: ![TMInfo]
  , _mi_isBeaten :: !Bool
  , _mi_atBeatenTimestamp :: !(Maybe Int)
  , _mi_atBeatenBy :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance ToJSON MapInfo where
  toJSON (MapInfo{..}) =
    Array $
      fromList
        [ toJSON _mi_trackId
        , toJSON _mi_trackUid
        , toJSON _mi_trackName
        , toJSON _mi_authorLogin
        , toJSON _mi_tags
        , toJSON _mi_mapType
        , toJSON _mi_authorTime
        , toJSON _mi_wr
        , toJSON _mi_nbPlayers
        , toJSON _mi_isHidden
        , toJSON _mi_reason
        , toJSON _mi_atSetByPlugin
        , toJSON _mi_reported
        , toJSON _mi_uploadedTimestamp
        , toJSON _mi_validation
        , toJSON _mi_fileSize
        , toJSON _mi_info
        , toJSON _mi_isBeaten
        , toJSON _mi_atBeatenTimestamp
        , toJSON _mi_atBeatenBy
        ]

instance FromJSON MapInfo where
  parseJSON v = 
    MapInfo
      <$> parseIndexedJSON parseJSON 0 v
      <*> parseIndexedJSON parseJSON 1 v
      <*> parseIndexedJSON parseJSON 2 v
      <*> parseIndexedJSON parseJSON 3 v
      <*> parseIndexedJSON parseJSON 4 v
      <*> parseIndexedJSON parseJSON 5 v
      <*> parseIndexedJSON parseJSON 6 v
      <*> parseIndexedJSON parseJSON 7 v
      <*> parseIndexedJSON parseJSON 8 v
      <*> parseIndexedJSON parseJSON 9 v
      <*> parseIndexedJSON parseJSON 10 v
      <*> parseIndexedJSON parseJSON 11 v
      <*> parseIndexedJSON parseJSON 12 v
      <*> parseIndexedJSON parseJSON 13 v
      <*> parseIndexedJSON parseJSON 14 v
      <*> parseIndexedJSON parseJSON 15 v
      <*> parseIndexedJSON parseJSON 16 v
      <*> parseIndexedJSON parseJSON 17 v
      <*> parseIndexedJSON parseJSON 18 v
      <*> parseIndexedJSON parseJSON 19 v
        -- [ toJSON _mi_trackId      
        -- , toJSON _mi_trackUid     
        -- , toJSON _mi_trackName    
        -- , toJSON _mi_authorLogin  
        -- , toJSON _mi_tags         
        -- , toJSON _mi_mapType      
        -- , toJSON _mi_authorTime   
        -- , toJSON _mi_wr
        -- , toJSON _mi_nbPlayers
        -- , toJSON _mi_isHidden
        -- , toJSON _mi_reason
        -- , toJSON _mi_atSetByPlugin
        -- , toJSON _mi_reported
        -- , toJSON _mi_uploadedTimestamp
        -- , toJSON _mi_validation
        -- , toJSON _mi_fileSize
        -- , toJSON _mi_info
        -- , toJSON _mi_isBeaten
        -- , toJSON _mi_atBeatenTimestamp
        -- , toJSON _mi_atBeatenBy
        -- ]


data MapInfoResponse
  = MapInfoResponse
  { _mir_keys :: ![Text]
  , _mir_tracks :: ![MapInfo]
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_mir_")} ''MapInfoResponse)

type TMXApi =
  "tmx"
    :> ( "unbeaten_ats" :> Cached 1200 (UnbeatenAtsResponse UnbeatenAtTrack) :> Get '[JSON] (UnbeatenAtsResponse UnbeatenAtTrack)
           :<|> "unbeaten_ats" :> "v2" :> Cached 1200 (UnbeatenAtsResponse UnbeatenAtTrack) :> Get '[JSON] (UnbeatenAtsResponse UnbeatenAtTrack)
           :<|> "unbeaten_ats" :> "leaderboard" :> Get '[JSON] UnbeatenAtsLeaderboardResponse
           :<|> "recently_beaten_ats" :> Cached 1200 RecentlyBeatenAtsResponse :> Get '[JSON] RecentlyBeatenAtsResponse
           :<|> "unbeaten_count" :> Get '[PlainText] Text
           :<|> "refresh_caches" :> Get '[JSON] NoContent
           :<|> "maps_info" :> ReqBody '[JSON] [TrackUid] :> Post '[JSON] MapInfoResponse
       )

data ReportMapPayload
  = ReportMapPayload
  { _rmp_reason :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_rmp_")} ''ReportMapPayload)

data GridWithMapsResponse
  = GridWithMapsResponse
  { _gwmr_grid :: Grid
  , _gwmr_maps :: MapInfoResponse
  }

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_gwmr_")} ''GridWithMapsResponse)

data PublishGridBody
  = PublishGridBody
  { _pg_mapIds :: [TrackUid]
  , _pg_name :: Text
  , _pg_size :: Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_pg_")} ''PublishGridBody)

type ManagementAPI =
  "management" :> ( "report_map" :> Capture "mapId" Int :> ReqBody '[JSON] ReportMapPayload :> Post '[JSON] NoContent
               :<|> "report_map" :> Capture "mapId" Int :> Delete '[JSON] NoContent
               :<|> "add_missing_map" :> Capture "mapId" Int :> Post '[JSON] NoContent
  )

type AuthAPI =
  "auth" :> "openplanet" :> ReqBody '[JSON] InternalAuth :> Post '[JSON] InternalAuth
    :<|> "auth" :> "is-trusted" :> Capture "accountId" Text :> Get '[JSON] Bool
    :<|> "auth" :> "fake-auth" :> Capture "accountId" Text :> Post '[JSON] InternalAuth

data HTML = HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ val = BS.fromStrict $ encodeUtf8 val

data ValidationReplayUpload
  = ValidationReplayUpload
  { _vru_tmxid :: !Int
  , _vru_replay :: !FilePath
  , _vru_public :: !Bool
  }
  deriving (Show)

instance FromMultipart Tmp ValidationReplayUpload where
  fromMultipart multipartData =
    ValidationReplayUpload
      <$> ( lookupInput "tmxid" multipartData >>= \x ->
              case readMaybe x of
                Nothing -> Left "Invalid TMXID"
                Just idx -> Right idx
          )
      <*> (fdPayload <$> lookupFile "replay" multipartData)
      <*> (pure $ either (const False) (const True) $ lookupInput "public" multipartData)

type HtmxAPI =
  "htmx"
    :> ( ("upload-replay" :> MultipartForm Tmp ValidationReplayUpload :> Post '[HTML] Text)
           :<|> ("map-by-tmxid" :> QueryParam' '[Required] "tmxid" Text :> Get '[HTML] Text)
       )

data ExportDBResponse
  = ExportDBResponse
  { _edr_maps :: IxEntry
  }

$(deriveToJSON defaultOptions{fieldLabelModifier = drop (Text.length "_edr_")} ''ExportDBResponse)

type GridAPI = "grids" :> (
        ( "active" :> Get '[JSON] [EnrichedGrid]
        :<|> "expired" :> Capture "userUid" Text :> Get '[JSON] [Grid]
        :<|> Capture "gridId" ID :> QueryParam "updatedAfter" PosixTS :> Get '[JSON] (Maybe Grid)
        :<|> Capture "gridId" ID :> "players" :> Get '[JSON] [(Text, Maybe TrackUid)]
        :<|> Capture "gridId" ID :> "with-maps" :> Get '[JSON] (Maybe GridWithMapsResponse))
    :<|> Auth '[JWT] AUser :> (
             Capture "gridId" ID :> "messages" :> ReqBody '[JSON] Text :> Post '[JSON] NoContent
        :<|> Capture "gridId" ID :> "ping-connected" :> QueryParam "mapUid" TrackUid :> Post '[JSON] NoContent
        :<|> Capture "gridId" ID :> "ping-disconnected" :> Post '[JSON] NoContent
        :<|> Capture "gridId" ID :> "maps" :> ReqBody '[JSON] [TrackUid] :> Post '[JSON] Grid
        :<|> ReqBody '[JSON] PublishGridBody :> Post '[JSON] Grid
        )
    )
  
type MapMonitorAPI = TMXApi :<|> DownloadMapAPI :<|> GridAPI :<|> (Auth '[JWT] AUser :> ManagementAPI) :<|> AuthAPI :<|> ("static" :> Raw) :<|> HtmxAPI :<|> ("db-dump" :> Get '[JSON] ExportDBResponse)
