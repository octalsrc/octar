{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Octar.Discard 
  ( runIndexNode

  ) where

import Octar.Index2
import Octar.Config
import Turtle.Ipfs (IpfsPath)

import Network.Discard
import Data.CARD
import Data.EventGraph (Edge,empty)
import Data.EventGraph.Ipfs (IpfsEG)
import System.Exit

import Control.Lens

-- This module is for ugly node setup stuff, if necessary

type R = IpfsEG I
type C = Edge R
type I = String
type S = RGArray IpfsPath

runIndexNode :: (IndexConfig, StorageConfig) 
             -> DManagerSettings C I S
             -> Script R C I S a 
             -> IO a
runIndexNode (i,s) settings script = case i^.indexPersist of
  Just sfile -> runNodeFile 
                  (i^.indexNodeId) 
                  (s^.storageApiPort) 
                  (i^.indexNetwork) 
                  sfile 
                  settings 
                  script
  Nothing -> do (a,_,_) <- runNode
                             (i^.indexNodeId) 
                             (s^.storageApiPort) 
                             (i^.indexNetwork)
                             mempty
                             Data.EventGraph.empty
                             settings
                             script
                return a
