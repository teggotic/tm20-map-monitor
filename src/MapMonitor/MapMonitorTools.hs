{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.MapMonitorTools (
  mapHasClones,
)
where

import Data.Aeson
import MapMonitor.Common
import MapMonitor.DB
import MapMonitor.MapCache
import Protolude
import RIO (HasLogFunc, displayShow, logError, tshow)
import qualified RIO.Text as Text
import System.Process.Typed
import UnliftIO

mapHasClones :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasS3Connection env) => TMMap -> m (Maybe Bool)
mapHasClones tmmap = do
  res <- tryAny $ do
    withMapFile tmmap \mapFile -> do
      readProcessStdout (proc "MapMonitorTools" ["has-clones", Text.unpack mapFile]) >>= \case
        (ExitSuccess, out) -> do
          case decode @Int out of
            Nothing -> do
              logError $ "Failed to parse MapMonitorTools has-clones output: " <> displayShow out
              return Nothing
            Just report -> do
              return $ Just $ report /= 0
        (ExitFailure _, (tshow -> out)) -> do
          logError $ "Failed to run MapMonitorTools has-clones validation" <> displayShow out
          return Nothing
  case res of
    Left err -> do
      putText $ "Error: " <> show err
      return Nothing
    Right (Just chk) -> return chk
    _ -> return Nothing
