{-# LANGUAGE NoImplicitPrelude #-}
module MapMonitor.API.Util
  ( runInClient
  )
where

import Protolude
import Servant.Client
import UnliftIO.Retry
import Control.Lens
import Network.HTTP.Types.Status
import RIO (logError, HasLogFunc, displayShow)


runInClient :: (MonadReader s m, MonadIO m, HasLogFunc s) => Getting ClientEnv s ClientEnv -> ClientM b -> m (Either ClientError b)
runInClient getEnv m = do
  env <- view getEnv
  retrying
    limitedBackoff
    (const $ \case
      Right _ -> return False
      Left (FailureResponse _ ((responseStatusCode) -> (statusCode -> 404))) -> return False
      Left e -> do
        logError $ "Error: " <> displayShow e
        return True
    )
    \_ -> do
      liftIO $ runClientM m env

limitedBackoff :: Monad m => RetryPolicyM m
limitedBackoff = exponentialBackoff (10^6) <> limitRetries 4
