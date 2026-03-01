{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.MissingItemsCheck where

import Data.Aeson
import Data.Aeson.TH
import MapMonitor.Common
import MapMonitor.DB
import MapMonitor.MapCache
import Protolude
import RIO (HasLogFunc, tshow)
import qualified RIO.Text as Text
import System.Process.Typed
import UnliftIO

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

checkMissingItems :: (MonadUnliftIO m, MonadReader env m, HasS3Connection env, HasLogFunc env) => TMMap -> m (Maybe MissingItemsResult)
checkMissingItems tmmap = do
  res <- tryAny $ do
    withMapFile tmmap \mapFile -> do
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
    Right (Just chk) -> return chk
    _ -> return Nothing
