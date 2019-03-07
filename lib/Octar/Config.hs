{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Octar.Config
  ( MultiConfig
  , loadMultiConfig
  , parseMultiConfig
  , validate
  , indexes
  , storages
  , defaultIndex
  , storageFor
  , storageFor'
  , indexWithStorage
  , indexNames
  , storageNames
  , indexesUsingStorage

  , IndexConfig
  , indexNetwork
  , indexPersist
  , indexNodeId
  , indexArchivist
  , indexStorageName

  , StorageConfig
  , storageApiPort
  , storageApiMultiAddr
  , storageApiHttp
  , storageGateway
  
  ) where

import Control.Monad.Except (throwError)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Yaml
import Network.Discard (NetConf)
import Control.Lens
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)

data StorageConfig = StorageConfig 
  { _storageApiPort :: Int
  , _storageGateway :: Maybe Int }

makeLenses ''StorageConfig

-- | Use the configured storage API port to make a localhost
-- "multiaddress", suitable for an IPFS CLI client interface
storageApiMultiAddr :: Getter StorageConfig String
storageApiMultiAddr = to $ \conf -> 
  "/ip4/127.0.0.1/tcp/" <> show (conf^.storageApiPort)

-- | Use the configured storage API port to make a localhost URI,
-- suitable for an IPFS HTTP client interface
storageApiHttp :: Getter StorageConfig String
storageApiHttp = to $ \conf ->
  "http://localhost:" <> show (conf^.storageApiPort)

instance FromJSON StorageConfig where
  parseJSON = withObject "StorageConfig" $ \v -> StorageConfig
    <$> v .: "api-port"
    <*> v .:? "gateway-port"

data IndexConfig = IndexConfig
  { _indexStorageName :: String
  , _indexNetwork :: NetConf String
  , _indexPersist :: Maybe FilePath
  , _indexNodeId :: String
  , _indexArchivist :: String }

makeLenses ''IndexConfig

instance FromJSON IndexConfig where
  parseJSON = withObject "IndexConfig" $ \v -> IndexConfig
    <$> v .: "storage"
    <*> v .: "network"
    <*> v .:? "save-file"
    <*> v .: "node-id"
    <*> v .: "archivist"

data MultiConfig = MultiConfig
  { _indexes :: Map String IndexConfig
  , _storages :: Map String StorageConfig
  , _defaultIndexName :: Maybe String }

makeLenses ''MultiConfig

instance FromJSON MultiConfig where
  parseJSON = withObject "MultiConfig" $ \v -> MultiConfig
    <$> v .:? "indexes" .!= mempty
    <*> v .:? "storages" .!= mempty
    <*> v .:? "main-index"

loadMultiConfig :: FilePath -> IO (Either String MultiConfig)
loadMultiConfig fp = do
  decodeFileEither fp >>= \case
    Right conf -> return $ validate conf
    Left e -> return $ Left (show e)

parseMultiConfig :: ByteString -> Either String MultiConfig
parseMultiConfig bs = do
  case decodeEither' bs of
    Right conf -> validate conf
    Left e -> Left $ show e

validate :: MultiConfig -> Either String MultiConfig
validate conf = do
  case conf ^. defaultIndexName of
    Just n | Map.member n (conf ^. indexes) -> return ()
           | otherwise -> throwError $ "Default index \"" <> n <> "\" does not exist."
    Nothing -> return ()
  let checkS :: IndexConfig -> Either String ()
      checkS i = if Map.member (i ^. indexStorageName) (conf ^. storages)
                    then return ()
                    else throwError $ "Storage \"" <> (i ^. indexStorageName) <> "\" does not exist."
  mapM_ checkS (conf ^. indexes)
  return conf

-- The getters from here out assume that the index has passed validation

defaultIndex :: Getter MultiConfig (Maybe IndexConfig)
defaultIndex = to $ \conf -> case conf^.defaultIndexName of
                               Just iname -> Just $ (conf^.indexes) Map.! iname
                               Nothing -> Nothing

storageFor' :: String -> Getter MultiConfig (Maybe StorageConfig)
storageFor' iname = to $ \conf -> case conf^.indexes.at iname of
                                    Just i -> conf^.storages.at (i^.indexStorageName)
                                    Nothing -> Nothing

storageFor :: Getter MultiConfig (Maybe IndexConfig) -> Getter MultiConfig (Maybe StorageConfig)
storageFor g = to $ \conf -> case conf^.g of
                               Just i -> Just . fromJust $ conf^.storages.at (i^.indexStorageName)
                               Nothing -> Nothing

indexWithStorage :: Getter MultiConfig (Maybe IndexConfig) 
                 -> Getter MultiConfig (Maybe (IndexConfig, StorageConfig))
indexWithStorage g = to $ \conf -> case (conf^.g, conf^.storageFor g) of
                                     (Just i, Just s) -> Just (i,s)
                                     _ -> Nothing

indexNames :: Getter MultiConfig [String]
indexNames = to $ \conf -> Map.keys (conf^.indexes)

storageNames :: Getter MultiConfig [String]
storageNames = to $ \conf -> Map.keys (conf^.storages)

indexesUsingStorage :: String -> Getter MultiConfig (Maybe [String])
indexesUsingStorage sname = to $ \conf -> case sname `elem` (conf^.storageNames) of
  True -> Just 
          . map fst 
          . filter (\(_,i) -> (i^.indexStorageName) == sname)
          . Map.assocs
          $ conf^.indexes
  False -> Nothing
