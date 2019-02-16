{-# LANGUAGE OverloadedStrings #-}

module Octar.Index2 where

import Control.Monad
import Network.Discard
import Lang.Carol
import qualified Octar.Index as Index1
import Octar.Entry
import Turtle.Ipfs
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

data LiveIndex c i = LiveIndex
  { liveConn :: ManagerConn c i (RGArray IpfsPath)
  , liveMetaCache :: Index1.MetaCache }

updateMC :: Index1.MetaCache -> RGArray IpfsPath -> IpfsME (Index1.MetaCache, [IpfsPath], [IpfsPath])
updateMC mc (RGArray paths) = do
  let rmd = Map.keys mc List.\\ paths
      new = paths List.\\ Map.keys mc
      -- Remove metacache entries that are no longer in the replicated
      -- ref-list
      mc' = Map.restrictKeys mc (Set.fromList paths)
  -- Add any new paths to the metacache
  mc'' <- foldM Index1.addMetaCache mc' paths  
  return (mc'', new, rmd)

-- | Load entries from the backing store into the metacache, returning
-- a list of new entries found and a list of entries removed
refreshIndex :: LiveIndex c i -> IpfsME (LiveIndex c i, [IpfsPath], [IpfsPath])
refreshIndex live = do
  (mc', new, rmd) <- updateMC (liveMetaCache live) 
                     =<< (l2 $ runCarolR (liveConn live) (query crT))
  return (live { liveMetaCache = mc' }, new, rmd)

-- | Initialize an index from a discard node
loadIndex :: ManagerConn c i (RGArray IpfsPath) -> IpfsME (LiveIndex c i)
loadIndex man = do
  (mc',_,_) <- updateMC mempty =<< (l2 $ runCarolR man (query crT))
  return $ LiveIndex man mc'

addToIndex :: LiveIndex c i -> Entry -> IO (LiveIndex c i)
addToIndex live e = do
  let (p,m) = entryPair e
  runCarolR (liveConn live) (issue.ef $ RGAppend p)
  return $ live { liveMetaCache = Map.insert p m (liveMetaCache live)}

rmFromIndex :: LiveIndex c i -> IpfsPath -> IO (LiveIndex c i)
rmFromIndex live p =
  if Map.member p (liveMetaCache live)
     then do runCarolR (liveConn live) (issue.ef $ RGRemove p)
             return live { liveMetaCache = Map.delete p (liveMetaCache live) }
     else return live
