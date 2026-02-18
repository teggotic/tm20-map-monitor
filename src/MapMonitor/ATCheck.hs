{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.ATCheck (
  checkAtSetByPlugin,
)
where

import Data.Aeson
import Data.Aeson.TH
import Protolude
import RIO (tshow, logError, displayShow, HasLogFunc)
import qualified RIO.Text as Text
import System.Process.Typed
import UnliftIO
import MapMonitor.MapCache
import MapMonitor.Common

data MapValidationCheckerReport
  = MapValidationCheckerReport
  { _mvcr_uid :: !Text
  , _mvcr_validated :: !Text
  , _mvcr_type :: !Text
  , _mvcr_note :: !(Maybe Text)
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_mvcr_")} ''MapValidationCheckerReport)

checkAtSetByPlugin :: (MonadUnliftIO m, MonadReader env m, HasAppSettings env, HasLogFunc env) => Int -> m (Maybe Bool)
checkAtSetByPlugin tmxId = do
  res <- tryAny $ do
    getMapFile tmxId
      >>= \case
        Nothing -> return Nothing
        Just mapFile -> do
          readProcessStdout (proc "MapValidationChecker" ["--single", Text.unpack mapFile, "--strict-gps"]) >>= \case
            (ExitSuccess, out) -> do
              case decode @MapValidationCheckerReport out of
                Nothing -> do
                  logError $ "Failed to parse MapValidationChecker output: " <> displayShow out
                  return Nothing
                Just report -> do
                  if _mvcr_validated report == "Yes"
                    then do
                      return $ Just False
                    else do
                      return $ Just True
            (ExitFailure _, (tshow -> out)) -> do
              logError $ "Failed to run AT validation" <> displayShow out
              return Nothing
  case res of
    Left err -> do
      putText $ "Error: " <> show err
      return Nothing
    Right chk -> return chk
