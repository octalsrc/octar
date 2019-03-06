{-# LANGUAGE OverloadedStrings #-}

module Octar.Gateway (octarGateway) where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Lens
import Control.Concurrent.STM (TVar,readTVarIO)
import Network.HTTP.Client (newManager,defaultManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

import Octar.Index (MetaCache)
import Octar
import Turtle.Ipfs (unsafeIpfsPath)

mk404 :: ByteString -> WaiProxyResponse
mk404 = WPRResponse . responseLBS status404 [("Content-Type","text/plain")]

renderPath :: [Text] -> BS.ByteString
renderPath = encodeUtf8 . mconcat . L.intersperse "/"

-- | Run a gateway for the archive's items.  This is a reverse proxy
-- that takes paths of the form @/{storage}/{cid}/...@ and sends them
-- to the IPFS gateway configured for that storage
octarGateway :: Int -- ^ Local port to listen on
             -> Map String (StorageConfig, TVar MetaCache) -- ^ 
             -> IO ()
octarGateway port storMap = run port . waiProxyTo proxy defaultOnExc 
                            =<< newManager defaultManagerSettings
  where proxy req = case pathInfo req of
          stor:ref:p -> case M.lookup (T.unpack stor) storMap of
            Just (c,tv) -> case c^.storageGateway of
              Just sPort -> do 
                mc <- readTVarIO tv
                if unsafeIpfsPath ref `M.member` mc
                   then let req' = req { rawPathInfo = renderPath ("ipfs":ref:p) }
                            dest = ProxyDest 
                                        "localhost"
                                        (read sPort)
                        in do print req'
                              return (WPRModifiedRequest req' dest)
                   else return $ mk404 "That item is not in the index (and may \
                                          \not exist at all)\n\nTry \
                                          \gateway.ipfs.io instead?"
              Nothing -> return $ mk404 "No gateway configured for that storage."
            Nothing -> return $ mk404 "No storage system by that name exists."
          _ -> return $ mk404 "Can't help you there."
