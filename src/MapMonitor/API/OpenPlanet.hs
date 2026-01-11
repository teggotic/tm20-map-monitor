{-# LANGUAGE TemplateHaskell #-}

module MapMonitor.API.OpenPlanet (
  OpenPlanetAuthValidateResponse (..),
  OpenPlanetAuthValidateRequest (..),
  openPlanetAuthValidate,
)
where

import Data.Aeson
import Data.Aeson.TH
import GHC.Exts (IsList (fromList))
import Protolude
import qualified RIO.Text as Text
import Servant.API
import Servant.Client
import Web.FormUrlEncoded (ToForm (..))

data OpenPlanetAuthValidateResponse
  = OpenPlanetAuthValidateResponse
  { _opavr_account_id :: Text
  , _opavr_display_name :: Text
  , _opavr_token_time :: Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_opavr_")} ''OpenPlanetAuthValidateResponse)

data OpenPlanetAuthValidateRequest
  = OpenPlanetAuthValidateRequest
  { _opavr_token :: Text
  , _opavr_secret :: Text
  }
  deriving (Show)

instance ToForm OpenPlanetAuthValidateRequest where
  toForm auth =
    fromList
      [ ("token", toQueryParam (_opavr_token auth))
      , ("secret", toQueryParam (_opavr_secret auth))
      ]

type OpenPlanetAPI =
  "api" :> "auth" :> "validate" :> ReqBody '[FormUrlEncoded] OpenPlanetAuthValidateRequest :> Post '[JSON] OpenPlanetAuthValidateResponse

openPlanetAPI :: Proxy OpenPlanetAPI
openPlanetAPI = Proxy

openPlanetAuthValidate = client openPlanetAPI
