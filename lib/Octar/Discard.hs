{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Octar.Discard 
  ( runIndexNode
  , runIndexNodeAwait
  , runIndexNode'

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

runIndexNode' :: (IndexConfig, StorageConfig) 
              -> DManagerSettings C I S
              -> Script R C I S a 
              -> IO a
runIndexNode' (i,s) settings script = case i^.indexPersist of
  Just sfile -> runNodeFile 
                  (i^.indexNodeId) 
                  (s^.storageApiPort) 
                  (i^.indexNetwork) 
                  sfile 
                  settings 
                  script
  Nothing -> runNode
               (i^.indexNodeId) 
               (s^.storageApiPort) 
               (i^.indexNetwork)
               settings
               script 

runIndexNode :: (IndexConfig, StorageConfig) -> Script R C I S a -> IO a
runIndexNode cs script = runIndexNode' cs defaultDManagerSettings script

runIndexNodeAwait :: (IndexConfig, StorageConfig) -> Script R C I S a -> IO a
runIndexNodeAwait cs script = do
  (settings,await) <- awaitNetwork defaultDManagerSettings (Just 1000000)
  runIndexNode' cs settings (\i man -> await >> script i man)
