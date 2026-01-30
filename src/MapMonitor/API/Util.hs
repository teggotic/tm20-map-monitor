module MapMonitor.API.Util
  ( runInClient
  )
where

import Protolude
import Servant.Client
import UnliftIO.Retry
import Control.Lens

runInClient :: (MonadReader s m, MonadIO m) => Getting ClientEnv s ClientEnv -> ClientM b -> m (Either ClientError b)
runInClient getEnv m = do
  env <- view getEnv
  retrying
    limitedBackoff
    (const $ return . isLeft)
    \_ -> do
      liftIO $ runClientM m env

limitedBackoff :: Monad m => RetryPolicyM m
limitedBackoff = exponentialBackoff 50000 <> limitRetries 10
