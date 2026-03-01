{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PingRPC where

import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.TH
import Data.Conduit.Network
import Network.JSONRPC
import Protolude hiding (atomically, threadDelay)
import RIO
import qualified RIO.Text as Text
import RIO.Time
import qualified System.ZMQ4 as ZMQ
import UnliftIO.Concurrent
import UnliftIO.STM

data MapBeatenPing
  = MapBeatenPing
  { _mbp_tmxId :: !Int
  , _mbp_uid :: !Text
  , _mbp_name :: !Text
  , _mbp_authorUid :: !Text
  , _mbp_wrHolder :: !Text
  , _mbp_wrTime :: !Int
  , _mbp_wrTimestamp :: !UTCTime
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_mbp_")} ''MapBeatenPing)

data MissingItemsMapDetectedPing
  = MissingItemsMapDetectedPing
  { _mimdp_tmxId :: !Int
  , _mimdp_uid :: !Text
  , _mimdp_name :: !Text
  , _mimdp_authorUid :: !Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_mbp_")} ''MissingItemsMapDetectedPing)

data PingRPCMessage
  = PMMapBeatenPing MapBeatenPing
  | PMMissingItemsMapDetectedPing MissingItemsMapDetectedPing
  deriving (Show)

$(deriveJSON defaultOptions ''PingRPCMessage)

instance ToRequest PingRPCMessage where
  requestMethod PMMapBeatenPing{} = "MapBeatenPing"
  requestMethod PMMissingItemsMapDetectedPing{} = "MissingItemsMapDetectedPing"

  requestIsNotif _ = False

instance FromRequest PingRPCMessage where
  parseParams = const $ Just $ parseJSON

instance FromResponse PingRPCMessage where
  parseResult = const $ Nothing

runPingServer :: (MonadUnliftIO m, MonadLoggerIO m) => TQueue PingRPCMessage -> m ()
runPingServer q = do
  forever $
    jsonrpcTCPServer V2 False (serverSettings 8083 "127.0.0.1") do
      let
        loop =
          ((fmap fromRequest) <$> receiveRequest) >>= \case
            Nothing -> return ()
            Just (Left err) -> do
              print err
              loop
            Just (Right msg) -> do
              atomically $ writeTQueue q msg
              loop
      loop

runPingClient :: (MonadUnliftIO m, MonadLoggerIO m) => TQueue PingRPCMessage -> m ()
runPingClient q = do
  jsonrpcTCPClient V2 False (clientSettings 8083 "127.0.0.1") do
    forever do
      x <- atomically (readTQueue q)
      void $ sendRequest @_ @PingRPCMessage @() x

runBroker :: (MonadIO m) => m ()
runBroker = do
  -- withRunInIO \run -> do
  liftIO $
    ZMQ.withContext \ctx -> do
      ZMQ.withSocket ctx ZMQ.XPub \xpubSock -> do
        ZMQ.withSocket ctx ZMQ.XSub \xsubSock -> do
          ZMQ.bind xsubSock "ipc:///tmp/map-monitor-sub.sock"
          ZMQ.bind xpubSock "ipc:///tmp/map-monitor-pub.sock"
          ZMQ.proxy xsubSock xpubSock Nothing

class HasPubRpcSocket env where
  pubRpcSocketL :: Lens' env (ZMQ.Socket ZMQ.Pub)

instance HasPubRpcSocket (ZMQ.Socket ZMQ.Pub) where
  pubRpcSocketL = id

class HasSubRpcSocket env where
  subRpcSocketL :: Lens' env (ZMQ.Socket ZMQ.Sub)

instance HasSubRpcSocket (ZMQ.Socket ZMQ.Sub) where
  subRpcSocketL = id

rpcSend :: (MonadReader env m, MonadIO m, HasPubRpcSocket env) => PingRPCMessage -> m ()
rpcSend msg = do
  sok <- view pubRpcSocketL
  liftIO $ ZMQ.send' sok [] $ encode msg

rpcRecv :: (MonadReader env m, MonadIO m, HasSubRpcSocket env) => m (Maybe PingRPCMessage)
rpcRecv = do
  sok <- view subRpcSocketL
  msg <- liftIO $ ZMQ.receive sok
  return $ decodeStrict msg

withPubSocket :: (MonadUnliftIO m) => (ZMQ.Socket ZMQ.Pub -> m b) -> m b
withPubSocket inner =
  withRunInIO \run -> do
    ZMQ.withContext \ctx -> do
      ZMQ.withSocket ctx ZMQ.Pub \sock -> do
        ZMQ.connect sock "ipc:///tmp/map-monitor-pub.sock"
        run $ inner sock

withSubSocket :: (MonadUnliftIO m) => (ZMQ.Socket ZMQ.Sub -> m b) -> m b
withSubSocket inner =
  withRunInIO \run -> do
    ZMQ.withContext \ctx -> do
      ZMQ.withSocket ctx ZMQ.Sub \sock -> do
        ZMQ.connect sock "ipc:///tmp/map-monitor-pub.sock"
        run $ inner sock
