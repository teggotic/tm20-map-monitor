{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (void, when)
import Control.Monad.Logger
import Data.Time.Format.ISO8601
import Protolude hiding (atomically, forkIO, isPrefixOf, threadDelay, toLower)
import RIO hiding (show)
import UnliftIO.Concurrent
import UnliftIO.STM

-- import qualified Data.Text.IO as TIO

import PingRPC
import RIO.Text

import Discord
import qualified Discord.Requests as R
import Discord.Types
import System.Environment

dsClient q = do
  dsToken <- liftIO $ getEnv "DISCORD_TOKEN"
  generalChat <- atomically $ newEmptyTMVar
  userFacingError <-
    runDiscord $
      def
        { discordToken = pack dsToken
        , discordOnEvent = eventHandler generalChat
        , discordOnLog = \s -> putText s >> putText ""
        , discordOnStart = startHandler q generalChat
        } -- if you see OnLog error, post in the discord / open an issue
  putText userFacingError

startHandler q chatTMVar = do
  liftIO $ putText "Bot started!"
  void $ forkIO $ do
    forever $ void $ tryAny do
      chat <- atomically $ readTMVar chatTMVar
      forever $ do
        msg <- atomically $ readTQueue q
        print msg
        case msg of
          (PMMapBeatenPing x@(MapBeatenPing{_mbp_authorUid = "c331bdbf-2182-4a51-813d-87d6f0f209c5"})) -> do
            print x
            void $ restCall $ R.CreateMessage (channelId chat) $ "Map [" <> _mbp_name x <> "](https://trackmania.exchange/mapshow" <> Protolude.show (_mbp_tmxId x) <> ") was beaten by [user](https://trackmania.io/#/player/" <> _mbp_wrHolder x <> ") with time of " <> show (_mbp_wrTime x) <> " at " <> pack (iso8601Show $ _mbp_wrTimestamp x) <> "."
            threadDelay (10 ^ 6)
          (PMMissingItemsMapDetectedPing x@(MissingItemsMapDetectedPing{_mimdp_uid = __mimdp_uid, _mimdp_tmxId = __mimdp_tmxId, _mimdp_name = __mimdp_name, _mimdp_authorUid = __mimdp_authorUid})) -> do
            print x
            void $
              restCall $
                R.CreateMessage (channelId chat) $
                  "Map [" <> _mimdp_name x <> "](https://trackmania.exchange/mapshow" <> Protolude.show (_mimdp_tmxId x) <> ") was marked as having missing items"
            threadDelay (10 ^ 6)
          _ -> pass

eventHandler chatTMVar event = do
  print event
  case event of
    MessageCreate m ->
      when (isPing m && not (fromBot m)) $ void $ forkIO $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        threadDelay (2 * 10 ^ 6)
        void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    GuildCreate (Guild{guildName = "teggot's server"}) GuildCreateData{guildCreateChannels = chans} -> do
      forM_ chans $ \case
        c@(ChannelText{channelName = "general"}) ->
          atomically $ putTMVar chatTMVar c
        _ -> return ()
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent

--
main :: IO ()
main = do
  pings <- atomically $ newTQueue

  withSubSocket \sok -> do
    void $ forkIO $ do
      forever $ do
        runReaderT rpcRecv sok >>= \case
          Nothing -> pure ()
          Just msg -> atomically $ writeTQueue pings msg

    dsClient pings
