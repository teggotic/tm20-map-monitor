{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module MapMonitor.ReplayValidation
  where

import MapMonitor.Integrations
import Protolude hiding ((<.>))
import MapMonitor.Common
import qualified Data.Map as Map
import qualified Data.UUID.V4 as UUID4
import qualified RIO.Text as Text
import Data.Aeson
import Data.Aeson.TH
import RIO.FilePath
import System.Process.Typed
import UnliftIO.Temporary
import MapMonitor.DB
import MapMonitor.API
import Control.Lens hiding ((<.>))
import Network.Minio
import Data.Aeson.Key
import RIO (toStrictBytes, logError, displayShow, HasLogFunc)
import UnliftIO
import RIO.Time (getCurrentTime)

data SimulationResult
  = SimulationResult
  { _sr_NbCheckpoints :: !Int
  , _sr_NbRespawns :: !Int
  , _sr_Time :: !Int
  , _sr_Score :: !Int
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_sr_")} ''SimulationResult)

data ValidationResult
  = ValidationResult
    { _vr_IsValid :: !Bool
    , _vr_Desc :: !(Maybe Text)
    , _vr_FileName :: !Text
    , _vr_MapUid :: !Text
    , _vr_ValidatedResult :: !(Maybe SimulationResult)
    }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop (Text.length "_vr_")} ''ValidationResult)

validateReplay :: (MonadUnliftIO m, MonadReader env m, HasState env, HasS3Connection env, HasLogFunc env) => ValidationReplayUpload -> m (Either Text (Maybe Text))
validateReplay req = do
  queryAcid (GetMapById $ TMXId $ _vru_tmxid $ req) >>= \case
    Nothing -> return $ Left $ "Map not found"
    Just tmmap -> do
      withSystemTempDirectory "map-monitor-validation" \dir -> do
        uuid <- liftIO UUID4.nextRandom
        let ghostPath = dir </> show uuid <.> "Ghost.Gbx"
        runProcess (proc "tm20-replay-validator" ["extract-ghost", _vru_replay $ req, ghostPath]) >>= \case
          ExitFailure _ -> do
            return $ Left $ "Failed to extract ghost from uploaded file"
          ExitSuccess -> do
            let mapFile = dir </> (Text.unpack $ _tmm_uid tmmap) <.> "Map.Gbx"
            let bucketPath = Text.pack $ "maps/uid" </> Text.unpack (_tmm_uid tmmap) <.> "Map.Gbx"
            conn <- view s3ConnL
            buck <- view s3BucketL
            (liftIO $ runMinioWith conn $ fGetObject buck bucketPath mapFile defaultGetObjectOptions) >>= \case
              Left err -> do
                return $ Left $ "Don't have map file: " <> show err
              Right _ -> do
                liftIO $ encodeFile (dir </> "mappings.json") (object [(fromString ghostPath, String $ _tmm_uid tmmap)])
                putText $ "Running validation on"
                readProcess (proc "tm20-replay-validator" ["validate-dir", dir]) >>= \case
                  (ExitFailure _, _, err) -> do
                    return $ Left $ "Validation failed: " <> decodeUtf8 (toStrictBytes err)
                  (ExitSuccess, out, _) -> do
                    case decode @(Map Text ValidationResult) out of
                      Just (Map.lookup (toS ghostPath) -> Just res) -> do
                        if _vr_IsValid res || maybe False ("wrong simu" `Text.isInfixOf`) (_vr_Desc res)
                           then case _vr_ValidatedResult res of
                             Nothing -> return $ Left $ "Something went wrong: " <> show res
                             Just sr ->
                               if _sr_Time sr == _tmm_authorMedal tmmap
                                 then do
                                   now <- getCurrentTime
                                   case (_vru_public req) of
                                     False -> do
                                       void $ withAcid1 updateMaps
                                        [ (defPatch $ _tmm_tmxId tmmap){_tmmp_validationReplay = Just $ Just (Nothing, now)}
                                        ]
                                       return $ Right Nothing
                                     True -> do
                                       uploaded <- liftIO $ runMinioWith conn do
                                         fPutObject "map-monitor-replays" ("ghosts/" <> (show uuid) <> ".Ghost.Gbx") ghostPath defaultPutObjectOptions
                                       case uploaded of
                                         Left err -> do
                                           logError $ "Error uploading replay: " <> displayShow err
                                           return $ Right Nothing
                                         Right _ -> do
                                           void $ withAcid1 updateMaps
                                            [ (defPatch $ _tmm_tmxId tmmap){_tmmp_validationReplay = Just $ Just (Just (show uuid), now)}
                                            ]
                                           return $ Right $ Just $ show uuid
                                 else return $ Left $ "Replay is valid, but replay length does not match AT"
                           else return $ Left $ "Validation failed: " <> show res
                      _ -> do
                        return $ Left $ "Something went wrong: " <> show out
