module PingRPC where

import RIO
import Protolude

pingRPC :: IO ()
pingRPC = putText "pong"
