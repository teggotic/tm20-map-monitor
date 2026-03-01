{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.API
where

import Data.Aeson
import Data.Aeson.TH
import MapMonitor.CachedAPIResponses
import MapMonitor.DB
import Network.HTTP.Media ((//), (/:))
import Protolude
import qualified RIO.ByteString as BS
import RIO.List
import RIO.Prelude.Types
import qualified RIO.Text as Text
import Servant.API
import Servant.Auth
import Servant.Auth.JWT
import Servant.Multipart

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

type DownloadMapAPI =
  "maps" :> "download" :> Capture "mapId" Int :> Raw

type TMXApi =
  "tmx"
    :> ( "unbeaten_ats" :> Get '[JSON] UnbeatenAtsResponse
           :<|> "unbeaten_ats" :> "leaderboard" :> Get '[JSON] UnbeatenAtsLeaderboardResponse
           :<|> "recently_beaten_ats" :> Get '[JSON] RecentlyBeatenAtsResponse
       )

data ReportMapPayload
  = ReportMapPayload
  { _rmp_reason :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_rmp_")} ''ReportMapPayload)

type ManagementAPI =
  ( "management" :> "report_map" :> Capture "mapId" Int :> ReqBody '[JSON] ReportMapPayload :> Post '[JSON] NoContent
      :<|> "management" :> "report_map" :> Capture "mapId" Int :> Delete '[JSON] NoContent
      :<|> "management" :> "add_missing_map" :> Capture "mapId" Int :> Post '[JSON] NoContent
  )

type AuthAPI =
  "auth" :> "openplanet" :> ReqBody '[JSON] InternalAuth :> Post '[JSON] InternalAuth
    :<|> "auth" :> "is-trusted" :> Capture "accountId" Text :> Get '[JSON] Bool

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

type MapMonitorAPI = TMXApi :<|> DownloadMapAPI :<|> (Auth '[JWT] AUser :> ManagementAPI) :<|> AuthAPI :<|> ("static" :> Raw) :<|> HtmxAPI :<|> ("db-dump" :> Get '[JSON] MapMonitorState)
