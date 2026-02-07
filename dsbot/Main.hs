{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude hiding (isPrefixOf, toLower, threadDelay)
import Control.Monad.Logger
import UnliftIO.STM
import RIO
import           Control.Monad (when, void)
import           UnliftIO.Concurrent
-- import qualified Data.Text.IO as TIO
import RIO.Text
import PingRPC

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

-- | Replies "pong" to every message that starts with "ping"
pingpongExample q = do
    userFacingError <- runDiscord $ def
             { discordToken = ""
             , discordOnEvent = eventHandler q
             , discordOnLog = \s -> putText s >> putText ""
             -- , discordOnStart = startHandler
             } -- if you see OnLog error, post in the discord / open an issue

    putText userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

-- startHandler :: DiscordHandler ()
-- startHandler = do
--   liftIO $ putStrLn "Bot started!"
--   threadDelay (2 * 10^6)
--   me <- readCache >>= return . cacheUser
--   cm <- restCall (R.CreateDM me)
--   case cm of
--     Left err -> liftIO (print err)
--     Right chan -> do
--       _ <- restCall (R.CreateMessage (channelId chan) "Hello from discord-haskell!")
--       pure ()
--
eventHandler :: TQueue PingRPCMessage -> Event -> DiscordHandler ()
eventHandler q event = do
  print event
  case event of
    Ready _ user _ _ _ _ _ -> do
      restCall (R.CreateDM (userId user))
        >>= \case
          Left err -> liftIO (print err)
          Right cm -> do
            void $ restCall (R.CreateMessage (channelId cm) "Hello from discord-haskell!")
    MessageCreate m -> when (isPing m && not (fromBot m)) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        threadDelay (2 * 10^6)
        void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent

--
main = do
  runStdoutLoggingT runServer
  -- pingpongExample
