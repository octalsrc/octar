module Octar.Live where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import qualified Data.Map as M

import Octar.Config
import Octar.Index (MetaCache)

data MultiLive = MultiLive
  { _liveConfig :: MultiConfig
  , _liveCache  :: Map String (TVar MetaCache) }

initMultiLive :: MultiConfig -> MultiLive
initMultiLive mc = MultiLive mc mempty
