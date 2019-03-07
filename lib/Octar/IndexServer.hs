{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Octar.IndexServer where

import Control.Concurrent.STM (TVar,readTVarIO)
import Control.Lens
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T

import Network.Wai
import Network.HTTP.Types (status200,status404)
import qualified Network.Wai.Handler.Warp as Warp (run)

import Octar
import Octar.CLI.Opts
import Octar.Index.Frontend.StaticWeb

data IServerConf = IServerConf
  { serverGateway :: String
  , serverPort :: Int }

-- | Make an 'IServerConf'
iserverConf :: String -- ^ Gateway to put on item links (i.e. https://gateway.ipfs.io)
            -> Int -- ^ Server port (i.e. 3001)
            -> IServerConf
iserverConf = IServerConf

-- | Serve a requested HTML webpage
ok resp = resp . responseLBS status200 [("Content-Type","text/html")]

-- | Serve a plain-text 404 message
no resp = resp . responseLBS status404 [("Content-Type","text/plain")]

-- | Serve a simple browsing interface for the mirrored indexes
runIndexServer :: IServerConf -> MultiLive -> IO ()
runIndexServer c ml = Warp.run (serverPort c) $ \req resp -> do
  case map T.unpack (pathInfo req) of
    [iname] -> case (ml^.liveCache.at iname, ml^.livePinStatus.at iname) of
      (Just cv, Just pv) -> do 
        cache <- readTVarIO cv
        pinStat <- (== 0) <$> readTVarIO pv
        let stor = ml^.liveConfig.indexes.at iname.to fromJust.indexStorageName
        ok resp (indexWebpage' (serverGateway c) stor cache pinStat)
      _ -> no resp "No index by that name exists."
    [] -> ok resp (mainPage (M.keys (ml^.liveConfig.indexes)))
    _ -> no resp "Can't help you there."

-- | Produce gateway URI from 'MirrorConf', if possible
indexGatewayUri :: MirrorConf -> Either String String
indexGatewayUri c = case mirrorGatewayExternalUri c of
  Just uri -> Right uri
  Nothing -> case mirrorGatewayPort c of
    Just port -> Right $ "http://localhost:" <> show port
    Nothing -> Left "Serving an index requires -e or -g."
