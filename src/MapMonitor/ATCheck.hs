{-# LANGUAGE TemplateHaskell #-}

module MapMonitor.ATCheck (
  checkAtSetByPlugin,
)
where

import Data.Aeson
import Data.Aeson.TH
import Protolude
import RIO (tshow)
import RIO.Prelude.Types
import qualified RIO.Text as Text
import System.Process.Typed
import UnliftIO

data MapValidationCheckerReport
  = MapValidationCheckerReport
  { _mvcr_uid :: Text
  , _mvcr_validated :: Text
  , _mvcr_type :: Text
  , _mvcr_note :: Maybe Text
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_mvcr_")} ''MapValidationCheckerReport)

checkAtSetByPlugin :: (MonadUnliftIO m) => Int -> m (Maybe Bool)
checkAtSetByPlugin tmxId = do
  res <- tryAny $ do
    withSystemTempDirectory "map-monitor-download" \dir -> do
      -- putText $ "Downloading map " <> show tmxId <> " to " <> Text.pack dir
      let outFile = Text.pack dir <> "/" <> show tmxId <> ".Map.Gbx"
      readProcess (proc "wget" ["-O", Text.unpack outFile, "https://trackmania.exchange/maps/download/" <> show tmxId, "-U", "teggot@proton.me; unbeaten ats project"])
        >>= \case
          (ExitSuccess, _, _) -> do
            -- putText $ "Downloaded map " <> outFile
            readProcessStdout (proc "MapValidationChecker" ["--single", Text.unpack outFile, "--strict-gps"])
              >>= \case
                (ExitSuccess, out) -> do
                  let result = decode @MapValidationCheckerReport out
                  case result of
                    Nothing -> do
                      putText $ "Failed to parse MapValidationChecker output: " <> tshow out
                      return Nothing
                    Just report -> do
                      if _mvcr_validated report == "Yes"
                        then do
                          -- putText $ "Map is valid"
                          return $ Just False
                        else do
                          -- putText $ "Map is invalid: " <> show (_mvcr_note report)
                          return $ Just True
                (ExitFailure _, (tshow -> out)) -> do
                  putText $ "Failed to run AT validation" <> out
                  return Nothing
          (ExitFailure _, _, _) -> do
            putText $ "Failed to download map " <> show tmxId
            return Nothing
  case res of
    Left err -> do
      putText $ "Error: " <> show err
      return Nothing
    Right chk -> return chk
