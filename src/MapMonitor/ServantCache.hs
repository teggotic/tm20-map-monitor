{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MapMonitor.ServantCache (
  CacheKey (..),
  CachedResponse (..),
  ResponseCache (..),
  Cached,
)
where

import Data.ByteString.Builder (toLazyByteString)
import Data.Cache (Cache)
import qualified Data.Cache as Cache
import Data.Type.Equality
import Network.HTTP.Types
import Network.Wai (Request (rawPathInfo, requestMethod), responseLBS)
import Network.Wai.Internal (Response (..))
import Protolude hiding (Handler)
import Servant
import Servant.Server
import Servant.Server.Internal.Delayed (runAction)
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router
import System.Clock (TimeSpec (TimeSpec))

data CacheKey = CacheKey
  { _ck_path :: !Text
  , _ck_method :: !Method
  }
  deriving (Show, Eq, Generic)

instance Hashable CacheKey

data CachedResponse = CachedResponse
  { _cr_response :: !LByteString
  , _cr_status :: !Status
  , _cr_headers :: !ResponseHeaders
  }
  deriving (Show)

newtype ResponseCache = ResponseCache (Cache CacheKey CachedResponse)

data Cached (n :: Nat) a

instance
  ( HasServer api context
  , KnownNat n
  , ServerT api m ~ (m a)
  , m ~ Servant.Handler
  , HasContextEntry context ResponseCache
  ) =>
  HasServer (Cached n a :> api) context
  where
  type ServerT (Cached n a :> api) m = ServerT api m

  route _ ctx action = choice cachedR computeR
   where
    ResponseCache cache = getContextEntry ctx

    cachedR = leafRouter \env request respond' -> do
      let
        cKey = CacheKey (decodeUtf8 $ rawPathInfo request) (requestMethod request)
      cachedResult <- Cache.lookup cache cKey
      runAction action env request respond' \_output -> do
        case cachedResult of
          Just cached ->
            Route $ responseLBS (_cr_status cached) (_cr_headers cached <> [("Servant-Cache-Status", "HIT")]) (_cr_response cached)
          Nothing -> Fail err406

    computeR = (\f req rsp -> f req (\res -> do cacheResponse req res; rsp res)) <$> route (Proxy :: Proxy api) ctx action

    cacheResponse :: Request -> RouteResult Response -> IO ()
    cacheResponse request (Route (ResponseBuilder status headers builder)) = do
      let cKey = (CacheKey (decodeUtf8 $ rawPathInfo request) (requestMethod request))
      Cache.insert' cache (Just $ TimeSpec (fromInteger $ natVal (Proxy @n)) 0) cKey (CachedResponse (toLazyByteString builder) status headers)
    cacheResponse _ _ = do
      pass

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s
