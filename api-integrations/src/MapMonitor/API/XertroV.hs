{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module MapMonitor.API.XertroV (
  XertrovMapMonitorNbPlayersResponse (..),
  HasXertrovClient (..),
  xertrovGetNbPlayers,
)
where

import Data.Aeson
import Data.Aeson.TH
import Protolude
import qualified RIO.Text as Text
import Servant.API
import Servant.Client
import Control.Lens

class HasXertrovClient env where
  xertrovClientL :: Lens' env ClientEnv

data XertrovMapMonitorNbPlayersResponse
  = XertrovMapMonitorNbPlayersResponse
  { _xmpr_created_ts :: !Int
  , _xmpr_last_highest_score :: !Int
  , _xmpr_last_update_started_ts :: !Int
  , _xmpr_nb_players :: !Int
  , _xmpr_uid :: !Text
  , _xmpr_updated_ts :: !Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_xmpr_")} ''XertrovMapMonitorNbPlayersResponse)

type XertrovMapMonitor =
  "map" :> Capture "mapId" Text :> "nb_players" :> Get '[JSON] XertrovMapMonitorNbPlayersResponse

xertrovAPI :: Proxy XertrovMapMonitor
xertrovAPI = Proxy

xertrovGetNbPlayers = client xertrovAPI
