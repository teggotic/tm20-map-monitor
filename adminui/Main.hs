{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where


import Control.Monad.IO.Class (liftIO)
import Data.Acid (AcidState)
import qualified Data.Acid as Acid
import Data.Aeson (FromJSON, ToJSON, Value (Array), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.IxSet.Typed as IxSet
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import GHC.Generics (Generic)
import MapMonitor.DB
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Data.Acid.Remote
import Network.Wai.Middleware.Cors (simpleCors, CorsResourcePolicy (..), simpleCorsResourcePolicy, cors)
import MapMonitor.ServantCache (ResponseCache (ResponseCache), Cached)
import Data.Cache (newCache, purge)
import System.Clock (TimeSpec(TimeSpec))
import Protolude (getArgs)

type AdminAPI =
  "admin" :> "api" :> "maps" :> Cached 1200 MapsResponse :> Get '[JSON] MapsResponse
    :<|> "admin" :> "api" :> "replace-map" :> ReqBody '[JSON] TMMap :> Post '[JSON] TMMap

data MapsResponse = MapsResponse
  { maps :: [TMMap]
  }
  deriving (Show, Eq, Generic)

instance ToJSON MapsResponse

instance FromJSON MapsResponse

adminApi :: Proxy AdminAPI
adminApi = Proxy

server :: ResponseCache -> AcidState MapMonitorState -> Server AdminAPI
server rc acid =
  getMapsHandler acid
    :<|> replaceMapHandler rc acid

getMapsHandler :: AcidState MapMonitorState -> Handler MapsResponse
getMapsHandler acid = do
  st <- liftIO $ Acid.query acid GetMapMonitorState
  let xs = IxSet.toAscList (Proxy @TMXId) (_mms_maps st)
  pure $ MapsResponse xs

replaceMapHandler :: ResponseCache -> AcidState MapMonitorState -> TMMap -> Handler TMMap
replaceMapHandler rc acid tm = do
  liftIO $ Acid.update acid (ReplaceMap tm)
  let ResponseCache cache = rc
  liftIO $ purge cache
  pure tm

app :: ResponseCache -> AcidState MapMonitorState -> Application
app rc acid =
  serveWithContext adminApi (rc :. EmptyContext) (server rc acid)

runAdminApi :: Int -> AcidState MapMonitorState -> IO ()
runAdminApi port acid = do
  cache <- newCache (Just $ TimeSpec 60 0)
  run port $
    cors (const $ Just policy) $
    (app (ResponseCache cache) acid)
  where
    policy =
      simpleCorsResourcePolicy
        { corsOrigins = Nothing
        , corsMethods = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        }

main :: IO ()
main = do
  port <- (read . Prelude.head) <$> getArgs
  acid <- openRemoteState @MapMonitorState skipAuthenticationPerform "localhost" port
  runAdminApi 8083 acid
