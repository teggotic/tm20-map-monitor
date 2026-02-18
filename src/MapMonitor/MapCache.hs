{-# LANGUAGE NoImplicitPrelude #-}
module MapMonitor.MapCache
where

import Protolude
import MapMonitor.Common
import Control.Lens
import qualified RIO.Text as Text
import System.Process.Typed
import RIO.Directory
import UnliftIO

getMapFile :: (MonadUnliftIO m, MonadReader env m, HasAppSettings env) => Int -> m (Maybe Text)
getMapFile tmxId = do
  res <- tryAny $ do
    _settings_mapsCacheDirectory <$> view appSettingsL
      >>= \case
        Nothing -> do
          withSystemTempDirectory "map-monitor-download" \dir -> do
            let outFile = Text.pack dir <> "/" <> show tmxId <> ".Map.Gbx"
            download outFile
        Just dir -> do
          let outFile = Text.pack dir <> "/" <> show tmxId <> ".Map.Gbx"
          liftIO (doesFileExist $ Text.unpack outFile) >>= \case
            True ->
              return $ Just outFile
            False -> do
              download outFile
  case res of
    Left err -> do
      putText $ "Error: " <> show err
      return Nothing
    Right chk -> return chk


 where
   download outFile = do
     readProcess (proc "wget" ["-O", Text.unpack outFile, "https://trackmania.exchange/maps/download/" <> show tmxId, "-U", "teggot@proton.me; unbeaten ats project"])
       >>= \case
          (ExitSuccess, _, _) -> do
            return $ Just outFile
          (ExitFailure _, _, _) -> do
            putText $ "Failed to download map " <> show tmxId
            return Nothing
