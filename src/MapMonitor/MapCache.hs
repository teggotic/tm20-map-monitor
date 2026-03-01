{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.MapCache
where

import Control.Lens hiding ((<.>))
import MapMonitor.Common
import MapMonitor.DB
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import Network.Minio
import Protolude hiding ((<.>))
import RIO (HasLogFunc, displayShow, logError)
import RIO.FilePath
import RIO.Text (pack, unpack)
import qualified RIO.Text as Text
import UnliftIO

withMapFile :: (MonadUnliftIO m, MonadReader env m, HasS3Connection env, HasLogFunc env) => TMMap -> (Text -> m a) -> m (Maybe a)
withMapFile tmmap@(TMMap{_tmm_tmxId = TMXId tmxId, _tmm_uid = uid}) action = do
  resE <- tryAny $ do
    withSystemTempDirectory "map-monitor-download" \dir -> do
      let outFile = dir </> show tmxId <.> "Map.Gbx"
      downloadMapFile tmmap outFile >>= \case
        Left err -> do
          logError $ "Error: " <> displayShow err
          return Nothing
        Right _ -> do
          Just <$> action (Text.pack outFile)

  case resE of
    Left err -> do
      logError $ "Error: " <> displayShow err
      return Nothing
    Right x -> return x

downloadMapFile :: (MonadUnliftIO m, MonadReader env m, HasS3Connection env, HasLogFunc env) => TMMap -> FilePath -> m (Either Text ())
downloadMapFile tmmap@(TMMap{_tmm_uid = uid}) outFile = do
  let mapPath = pack $ "maps/uid" </> unpack uid <.> "Map.Gbx"
  buck <- view s3BucketL
  conn <- view s3ConnL
  downloadTmxMapToS3 tmmap >>= \case
    Left err -> do
      logError $ "Error: " <> displayShow err
      return $ Left err
    Right _ -> do
      (liftIO $ runMinioWith conn $ fGetObject buck mapPath outFile defaultGetObjectOptions) >>= \case
        Left err -> do
          logError $ "Error: " <> displayShow err
          return $ Left $ show err
        Right _ -> do
          return $ Right ()

downloadTmxMapToS3 :: (MonadReader env m, HasS3Connection env, HasLogFunc env, MonadUnliftIO m) => TMMap -> m (Either Text Bool)
downloadTmxMapToS3 (TMMap{_tmm_tmxId = TMXId tmxId, _tmm_uid = uid}) = do
  resE <- tryAny do
    conn <- view s3ConnL
    buck <- view s3BucketL
    let mapPath = (pack $ "maps/uid" </> unpack uid RIO.FilePath.<.> ".Map.Gbx")
    statE <- liftIO $ runMinioWith conn do
      statObject buck mapPath defaultGetObjectOptions
    case statE of
      Left _ -> do
        runReq defaultHttpConfig do
          resE <- reqBr GET (https "trackmania.exchange" /: "mapgbx" /~ tmxId) NoReqBody (header "User-Agent" "teggot@proton.me; unbeaten-maps-monitor project") $ \r -> do
            liftIO $ runMinioWith conn $ do
              putObject buck mapPath (responseBodySource r) Nothing defaultPutObjectOptions
          case resE of
            Left err -> do
              return $ Left err
            Right _ -> do
              return $ Right False
      Right _ -> do
        return $ Right True
  case resE of
    Left err -> do
      logError $ "Error: " <> displayShow err
      return $ Left $ show err
    Right (Left err) -> do
      logError $ "Error: " <> displayShow err
      return $ Left $ show err
    Right (Right x) -> do
      return $ Right x
