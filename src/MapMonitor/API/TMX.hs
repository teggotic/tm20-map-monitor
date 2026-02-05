{-# LANGUAGE TemplateHaskell #-}

module MapMonitor.API.TMX (
  TMXSearchMapsFields (..),
  TMXSearchMapsMedals (..),
  TMXSearchMapsTag (..),
  TMXSearchMapsMap (..),
  TMXSearchMapsResponse (..),
  TMXSearchMaps (..),
  HasTMXClient (..),
  tmxSearchMaps,
  tmxMapToTMMap,
)
where

import Data.Aeson
import Data.Aeson.TH
import MapMonitor.DB
import Protolude
import qualified RIO.Text as Text
import Servant.API
import Servant.Client
import Control.Lens

class HasTMXClient env where
  tmxClientL :: Lens' env ClientEnv

data TMXSearchMapsFields
  = TMXSearchMapsFields [Text]
  deriving (Show)

instance ToHttpApiData TMXSearchMapsFields where
  toUrlPiece (TMXSearchMapsFields fields) = Text.intercalate "," fields

data TMXSearchMapsMedals
  = TMXSearchMapsMedals
  { _tmxsm_Author :: Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmxsm_")} ''TMXSearchMapsMedals)

data TMXSearchMapsTag
  = TMXSearchMapsTag
  { _tmxsmt_TagId :: Int
  , _tmxsmt_Name :: Text
  , _tmxsmt_Color :: Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmxsmt_")} ''TMXSearchMapsTag)

data TMXSearchMapsMap
  = TMXSearchMapsMap
  { _tmxsm_MapId :: Int
  , _tmxsm_MapUid :: Text
  , _tmxsm_Name :: Text
  , _tmxsm_Medals :: TMXSearchMapsMedals
  , _tmxsm_Tags :: [TMXSearchMapsTag]
  , _tmxsm_MapType :: Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmxsm_")} ''TMXSearchMapsMap)

data TMXSearchMapsResponse
  = TMXSearchMapsResponse
  { _tmxsr_More :: Bool
  , _tmxsr_Results :: [TMXSearchMapsMap]
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmxsr_")} ''TMXSearchMapsResponse)

type TMXAPI =
  "api"
    :> ( "maps"
           :> QueryParams "id" Int
           :> QueryParam' '[Required] "fields" TMXSearchMapsFields
           :> QueryParam "count" Int
           :> QueryParam "after" Int
           :> QueryParam "from" Int
           :> QueryParam "order1" Int
           :> Get '[JSON] TMXSearchMapsResponse
       )

tmxAPI :: Proxy TMXAPI
tmxAPI = Proxy

tmxSearchMaps' = client tmxAPI

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

tmxSearchMapsFields :: TMXSearchMapsFields
tmxSearchMapsFields = TMXSearchMapsFields ["MapId", "MapUid", "Name", "Medals.Author", "Tags", "MapType"]

data TMXSearchMaps
  = TMXSearchMaps
  { _tmxsm_ids :: [Int]
  , _tmxsm_count :: Maybe Int
  , _tmxsm_after :: Maybe Int
  , _tmxsm_from :: Maybe Int
  , _tmxsm_order1 :: Maybe Int
  }
  deriving (Show)

tmxSearchMaps :: TMXSearchMaps -> ClientM TMXSearchMapsResponse
tmxSearchMaps tmxs = tmxSearchMaps' (_tmxsm_ids tmxs) tmxSearchMapsFields (_tmxsm_count tmxs) (_tmxsm_after tmxs) (_tmxsm_from tmxs) (_tmxsm_order1 tmxs)
