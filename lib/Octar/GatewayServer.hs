{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Octar.GatewayServer (octarGateway) where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
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


octarGateway :: Int -> MultiLive -> IO ()
octarGateway port ml = run port . waiProxyTo proxy defaultOnExc 
                       =<< newManager defaultManagerSettings
  where proxy req = case pathInfo req of
          stor:ref:p -> case storageRefs (T.unpack stor) ml of
            Nothing -> return notConfigured
            Just getRefs -> getRefs >>= \case
              refs | unsafeIpfsPath ref `elem` refs -> 
                     case ml^.liveConfig.storages.at (T.unpack stor).to fromJust.storageGateway of
                       Just sPort -> 
                         let req' = req { rawPathInfo = renderPath ("ipfs":ref:p)
                                        , pathInfo = "ipfs":ref:p }
                             dest = ProxyDest "localhost" (sPort)
                             upLoc = map $ \case
                               ("Location",loc) -> 
                                 let stor8 = "/" <> encodeUtf8 stor <> "/"
                                     loc' = case BS.stripPrefix "/ipfs/" loc of
                                              Just item -> stor8 <> item
                                              Nothing -> loc
                                 in ("Location",loc')
                               h -> h
                         in return (WPRModifiedRequest req' dest upLoc)
                       Nothing -> return notConfigured
                   | otherwise -> return itemNotFound
          _ -> return $ mk404 "Can't help you there."

notConfigured = mk404 ("That is not a configured storage.")

itemNotFound = mk404 "That item is not in any index \
                     \served by this gateway.  It may \
                     \still exist.  Try an open gateway \
                     \such as https://gateway.ipfs.io."
