{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.CachedAPIResponses (
  UnbeatenAtsResponse (..),
  UnbeatenAtTrack,
  BeatenAtTrack,
  RecentlyBeatenAtsTracks (..),
  RecentlyBeatenAtsResponse (..),
  UnbeatenAtsLeaderboardResponse (..),
)
where

import Data.Aeson
import Data.Aeson.TH
import MapMonitor.DB
import RIO.List
import RIO.Prelude.Types
import Data.Fixed

type UnbeatenAtTrack = (TMXId, Text, Text, Text, Text, Text, Int, Int, Double, Int, Bool, Text, Bool, [(Text, Text)], Pico)

data UnbeatenAtsResponse
  = UnbeatenAtsResponse
  { _uar_keys :: ![Text]
  , _uar_tracks :: ![UnbeatenAtTrack]
  , _uar_nbTracks :: !Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length @[] "_uar_")} ''UnbeatenAtsResponse)

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
