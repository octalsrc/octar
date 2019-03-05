module Octar.GatewayProxy where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack,unpack)

import Control.Concurrent.STM (readTVarIO)
import Network.HTTP.Client (newManager,defaultManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

import Octar

gatewayProxy :: Int -> Map String (StorageConfig, TVar MetaCache) -> IO ()
gatewayProxy port storMap = run port . waiProxyTo proxy defaultOnExc 
                            =<< newManager defaultManagerSettings
  where proxy req = case pathInfo req of
          stor:ref:p -> case Map.lookup stor storMap of
            Just (c,tv) -> 
              do mc <- readTVarIO
                 if unsafeIpfsPath ref `Map.member` mc
                    then do let p' = ref:p
                            return (WPRModifiedRequest )
                    else undefined
