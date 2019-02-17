{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Octar.Index2 where

import Control.Monad
import Lang.Carol
import qualified Octar.Index as Index1
import Octar.Entry
import Turtle.Ipfs
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

type Index = RGArray IpfsPath

data LiveIndex c = LiveIndex
  { liveCC :: c
  , liveMetaCache :: Index1.MetaCache }

updateMC :: Index1.MetaCache -> Index -> IpfsME (Index1.MetaCache, [IpfsPath], [IpfsPath])
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
refreshIndex :: (CCarrier c Index IO) => LiveIndex c -> IpfsME (LiveIndex c, [IpfsPath], [IpfsPath])
refreshIndex live = do
  (mc', new, rmd) <- updateMC (liveMetaCache live) 
                     =<< (l2 $ carol (liveCC live) queryT)
  return (live { liveMetaCache = mc' }, new, rmd)

-- | Initialize an index from a discard node
loadIndex :: (CCarrier c Index IO) => c -> IpfsME (LiveIndex c)
loadIndex cc = do
  (mc',_,_) <- updateMC mempty =<< (l2 $ carol cc queryT)
  return $ LiveIndex cc mc'

addToIndex :: (CCarrier c Index IO) => LiveIndex c -> Entry -> IO (LiveIndex c)
addToIndex live e = do
  let (p,m) = entryPair e
  carol (liveCC live) $ issue (ef$ RGAppend p)
  return $ live { liveMetaCache = Map.insert p m (liveMetaCache live)}

rmFromIndex :: (CCarrier c Index IO) => LiveIndex c -> IpfsPath -> IO (LiveIndex c)
rmFromIndex live p =
  if Map.member p (liveMetaCache live)
     then do carol (liveCC live) $ issue (ef$ RGRemove p)
             return live { liveMetaCache = Map.delete p (liveMetaCache live) }
     else return live
