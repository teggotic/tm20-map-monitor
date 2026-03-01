{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.API.TMX (
  TMXSearchMapsFields (..),
  TMXSearchMapsMedals (..),
  TMXSearchMapsTag (..),
  TMXSearchMapsMap (..),
  TMXSearchMapsResponse (..),
  TMXSearchMaps (..),
  HasTMXClient (..),
  tmxSearchMaps,
)
where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Protolude
import qualified RIO.Text as Text
import Servant.API
import Servant.Client

class HasTMXClient env where
  tmxClientL :: Lens' env ClientEnv

data TMXSearchMapsFields
  = TMXSearchMapsFields ![Text]
  deriving (Show)

instance ToHttpApiData TMXSearchMapsFields where
  toUrlPiece (TMXSearchMapsFields fields) = Text.intercalate "," fields

data TMXSearchMapsMedals
  = TMXSearchMapsMedals
  { _tmxsm_Author :: !Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmxsm_")} ''TMXSearchMapsMedals)

data TMXSearchMapsTag
  = TMXSearchMapsTag
  { _tmxsmt_TagId :: !Int
  , _tmxsmt_Name :: !Text
  , _tmxsmt_Color :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmxsmt_")} ''TMXSearchMapsTag)

data TMXSearchMapsMap
  = TMXSearchMapsMap
  { _tmxsm_MapId :: !Int
  , _tmxsm_MapUid :: !Text
  , _tmxsm_Name :: !Text
  , _tmxsm_Medals :: !TMXSearchMapsMedals
  , _tmxsm_Tags :: ![TMXSearchMapsTag]
  , _tmxsm_MapType :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmxsm_")} ''TMXSearchMapsMap)

data TMXSearchMapsResponse
  = TMXSearchMapsResponse
  { _tmxsr_More :: !Bool
  , _tmxsr_Results :: ![TMXSearchMapsMap]
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

tmxSearchMapsFields :: TMXSearchMapsFields
tmxSearchMapsFields = TMXSearchMapsFields ["MapId", "MapUid", "Name", "Medals.Author", "Tags", "MapType"]

data TMXSearchMaps
  = TMXSearchMaps
  { _tmxsm_ids :: ![Int]
  , _tmxsm_count :: !(Maybe Int)
  , _tmxsm_after :: !(Maybe Int)
  , _tmxsm_from :: !(Maybe Int)
  , _tmxsm_order1 :: !(Maybe Int)
  }
  deriving (Show)

tmxSearchMaps :: TMXSearchMaps -> ClientM TMXSearchMapsResponse
tmxSearchMaps tmxs = tmxSearchMaps' (_tmxsm_ids tmxs) tmxSearchMapsFields (_tmxsm_count tmxs) (_tmxsm_after tmxs) (_tmxsm_from tmxs) (_tmxsm_order1 tmxs)
