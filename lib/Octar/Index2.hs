{-# LANGUAGE OverloadedStrings #-}

module Octar.Index2 where

import Network.Discard
import Data.EventGraph
import Data.EventGraph.Ipfs
import Data.CARD
import qualified Octar.Index as Index1
import Turtle.Ipfs


data LiveIndex = LiveIndex
  { liveConn :: ManagerConn (Edge (IpfsEG String)) String (RGArray IpfsPath)
  , liveMetaCache :: Index1.MetaCache }

-- | Load entries from the backing store into the metacache, returning
-- a list of new entries found
loadIndex :: LiveIndex -> IO (LiveIndex, [IpfsPath])
loadIndex = undefined
