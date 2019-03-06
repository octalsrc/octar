{-# LANGUAGE TemplateHaskell #-}

module Octar.Live
  ( MultiLive
  , liveConfig
  , liveCache
  , buildLive
  , indexRefs
  , storageRefs

  ) where

import Control.Concurrent.STM (TVar,readTVarIO)
import Control.Lens
import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Octar.Config
import Octar.Index (MetaCache)
import Turtle.Ipfs (IpfsPath)

data MultiLive = MultiLive
  { _liveConfig :: MultiConfig
  , _liveCache  :: Map String (TVar MetaCache) }

makeLenses ''MultiLive

initMultiLive :: MultiConfig -> MultiLive
initMultiLive mc = MultiLive mc mempty

buildLive :: (String -> MultiConfig -> IO (TVar MetaCache)) 
          -> MultiConfig 
          -> IO MultiLive
buildLive f mc = foldM (\ml iname -> do tv <- f iname mc :: IO (TVar MetaCache)
                                        return (ml & liveCache . at iname ?~ tv))
                       (initMultiLive mc :: MultiLive) 
                       (mc^.indexNames :: [String])

indexRefs :: String -> MultiLive -> Maybe (IO [IpfsPath])
indexRefs iname ml = (\tv -> M.keys <$> readTVarIO tv) <$> (ml^.liveCache.at iname)

storageRefs :: String -> MultiLive -> Maybe (IO [IpfsPath])
storageRefs sname ml = case ml^.liveConfig.indexesUsingStorage sname of
  Nothing -> Nothing
  Just inames -> Just $ fmap concat . sequence $ (fromJust <$> map (\iname -> indexRefs iname ml) inames)
