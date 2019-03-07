{-# LANGUAGE TemplateHaskell #-}

module Octar.Live
  ( MultiLive
  , liveConfig
  , liveCache
  , livePinStatus
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
  , _liveCache  :: Map String (TVar MetaCache)
  , _livePinStatus :: Map String (TVar Int) }

makeLenses ''MultiLive

initMultiLive :: MultiConfig -> MultiLive
initMultiLive mc = MultiLive mc mempty mempty

buildLive :: (String -> MultiConfig -> IO (TVar MetaCache, TVar Int, a)) 
          -> MultiConfig 
          -> IO (MultiLive, [a])
buildLive f mc = foldM (\(ml,rs) iname -> do (tv,pinV,r) <- f iname mc
                                             return ( ml & liveCache . at iname ?~ tv
                                                         & livePinStatus . at iname ?~ pinV
                                                    , r:rs ))
                       (initMultiLive mc, []) 
                       (mc^.indexNames)

indexRefs :: String -> MultiLive -> Maybe (IO [IpfsPath])
indexRefs iname ml = (\tv -> M.keys <$> readTVarIO tv) <$> (ml^.liveCache.at iname)

storageRefs :: String -> MultiLive -> Maybe (IO [IpfsPath])
storageRefs sname ml = case ml^.liveConfig.indexesUsingStorage sname of
  Nothing -> Nothing
  Just inames -> Just $ fmap concat . sequence $ (fromJust <$> map (\iname -> indexRefs iname ml) inames)
