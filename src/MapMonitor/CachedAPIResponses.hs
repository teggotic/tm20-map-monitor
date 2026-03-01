{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.CachedAPIResponses (
  UnbeatenAtsResponse (..),
  UnbeatenAtTrack (..),
  BeatenAtTrack,
  RecentlyBeatenAtsTracks (..),
  RecentlyBeatenAtsResponse (..),
  UnbeatenAtsLeaderboardResponse (..),
)
where

import Data.Aeson
import Data.Aeson.TH
import Data.Fixed
import GHC.Exts
import GHC.Generics
import MapMonitor.DB
import Protolude
import Protolude.Error (error)
import RIO.List
import RIO.Prelude.Types

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
        ]

data UnbeatenAtsResponse
  = UnbeatenAtsResponse
  { _uar_keys :: ![Text]
  , _uar_tracks :: ![UnbeatenAtTrack]
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
