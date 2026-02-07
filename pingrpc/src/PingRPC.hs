{-# LANGUAGE TemplateHaskell #-}
module PingRPC where

import RIO
import Data.Aeson
import Data.Aeson.TH
import Protolude hiding (threadDelay)
import Network.JSONRPC
import Data.Conduit.Network
import Control.Monad.Logger
import UnliftIO.STM
import UnliftIO.Concurrent
import qualified RIO.Text as Text

data MapBeatenPing
  = MapBeatenPing
  { _mbp_tmxId :: Int
  , _mbp_uid :: Text
  , _mbp_name :: Text
  , _mbp_wrTime :: Int
  , _mbp_wrTimestamp :: Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_mbp_")} ''MapBeatenPing)

data PingRPCMessage
  = PMMapBeatenPing MapBeatenPing

$(deriveJSON defaultOptions ''PingRPCMessage)

instance ToRequest PingRPCMessage where
  requestMethod PMMapBeatenPing {} = "MapBeatenPing"
  requestIsNotif PMMapBeatenPing {} = True

instance FromRequest PingRPCMessage where
  parseParams = const $ Just $ parseJSON

instance FromResponse PingRPCMessage where
  parseResult = const $ Nothing

runServer :: (MonadUnliftIO m, MonadLoggerIO m) => m ()
runServer = do
  jsonrpcTCPServer V2 False (serverSettings 8083 "127.0.0.1") do
    ((fmap fromRequest) <$> receiveRequest) >>= \case
      Nothing -> pass
      Just (Left err) -> do
        print err
      Just (Right msg) -> do
        case msg of
          PMMapBeatenPing mbp -> do
            print mbp

runClient :: (MonadUnliftIO m, MonadLoggerIO m) => m ()
runClient = do
  jsonrpcTCPClient V2 False (clientSettings 8083 "127.0.0.1") do
    forever $ do
      sendRequest @_ @PingRPCMessage @() (PMMapBeatenPing $ MapBeatenPing 0 "" "" 0 0)
        >>= \case
          Nothing -> pass
          Just x -> print x
      threadDelay $ 2 * 10^6
