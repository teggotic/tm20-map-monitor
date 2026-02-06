{-# LANGUAGE TemplateHaskell #-}
module MapMonitor.MissingItemsCheck where

import MapMonitor.MapCache
import MapMonitor.Common
import Data.Aeson
import Data.Aeson.TH
import Protolude
import qualified RIO.Text as Text
import UnliftIO
import System.Process.Typed
import RIO (tshow)


data MissingItemsReport
  = MissingItemsReport
  { _mir_mapUid :: !Text
  , _mir_hasProperlyEmbeddedBlocks :: !Bool
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_mir_")} ''MissingItemsReport)

data MissingItemsResult
  = NoMissingItems
  | MissingItems
  deriving (Show)

checkMissingItems :: (MonadUnliftIO m, MonadReader env m, HasAppSettings env) => Int -> m (Maybe MissingItemsResult)
checkMissingItems tmxId = do
  res <- tryAny $ do
    getMapFile tmxId
      >>= \case
        Nothing -> return Nothing
        Just mapFile -> do
          readProcessStdout (proc "EmbedsChecker" [Text.unpack mapFile]) >>= \case
            (ExitSuccess, out) -> do
              case decode @MissingItemsReport out of
                Nothing -> do
                  putText $ "Failed to parse EmbedsChecker output: " <> tshow out
                  return Nothing
                Just report -> do
                  return $ Just $ bool MissingItems NoMissingItems $ _mir_hasProperlyEmbeddedBlocks report
            (ExitFailure code, (tshow -> out)) -> do
              putText $ show code <> "; Failed to run AT validation" <> out
              return Nothing
  case res of
    Left err -> do
      putText $ "Error: " <> show err
      return Nothing
    Right chk -> return chk
