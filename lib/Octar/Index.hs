{-# LANGUAGE OverloadedStrings #-}

module Octar.Index 
  ( IndexIndex (..)
  , selectIndex
  , IndexConfig (..)
  , DirConfig (..)
  , pfPath
  , rmPath
  , mcPath
  , loadPF
  , writePF
  , appPF
  , IndexLive (..)
  , withIndex
  , addToIndex
  , rmFromIndex
  , writeIndex
  , MetaCache (..)
  ) where

import Prelude hiding (FilePath)

import Control.Monad.Except (throwError,liftEither,foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml
import Data.Aeson (pairs,foldable)
import Turtle hiding (cat)

import Turtle.Ipfs
import Octar.Metadata
import Octar.Entry

defaultGateway :: Text
defaultGateway = "https://ipfs.io"

-- | A list of index configurations, possibly with a default
data IndexIndex = IndexIndex 
  { indexDefault :: Maybe Text
  , indexIndex :: Map Text IndexConfig }
  deriving (Eq,Ord)

instance FromJSON IndexIndex where
  parseJSON = withObject "IndexIndex" $ \v -> IndexIndex
    <$> v .:? "default"
    <*> v .: "indexes"

selectIndex :: Maybe Text -> IndexIndex -> Either Text IndexConfig
selectIndex m (IndexIndex idef ii) = case m of
  Just t -> case Map.lookup t ii of
    Just i -> Right i
    Nothing -> Left ("Requested index " <> t <> " is not configured.")
  Nothing -> case (idef >>= \d -> Map.lookup d ii) of
    Just i -> Right i
    Nothing -> Left ("No valid default index.")

data IndexConfig = IndexConfig
  { indexConfigPath :: FilePath -- ^ The directory where pinfile and
                                -- cache live
  , indexConfigApi :: Maybe Text -- ^ The API where this index's
                                 -- entries are reachable
  , indexConfigDirs :: [DirConfig] -- ^ Places to write a directory
                                   -- upon modifying the index
  , indexArchivistName :: Text -- ^ Name with which to publish
  }
  deriving (Eq,Ord)

instance FromJSON IndexConfig where
  parseJSON = withObject "IndexConfig" $ \v -> IndexConfig
    <$> (fmap fromText (v .: "path"))
    <*> v .:? "api"
    <*> v .:? "directories" .!= []
    <*> v .: "archivist"

data DirConfig = DirConfig 
  { dirConfigPath :: FilePath
  , dirConfigGateway :: Text }
  deriving (Eq,Ord)

instance FromJSON DirConfig where
  parseJSON = withObject "DirConfig" $ \v -> DirConfig
    <$> (fmap fromText (v .: "path"))
    <*> v .:? "gateway" .!= defaultGateway

data IndexLive = IndexLive
  { liveConfig :: IndexConfig
  , liveMetaCache :: MetaCache
  , liveRmPaths :: [IpfsPath] }

pfPath :: IndexConfig -> FilePath
pfPath cfg = indexConfigPath cfg <> "pinfile"

rmPath :: IndexConfig -> FilePath
rmPath cfg = indexConfigPath cfg <> "rmfile"

mcPath :: IndexConfig -> FilePath
mcPath cfg = indexConfigPath cfg <> "metacache"

loadPF :: FilePath -> IO (Either Text [IpfsPath])
loadPF f = do ls <- Text.lines <$> readTextFile f
              return $ mapM mkIpfsPath ls

writePF :: [IpfsPath] -> FilePath -> IO ()
writePF ps f = 
  writeTextFile f (mconcat (map ((<> "\n") . fIpfsPath) ps))

appPF :: FilePath -> IpfsPath -> IO ()
appPF f p = append f (return $ lIpfsPath p)

makeExistF :: FilePath -> IO ()
makeExistF f = do exists <- testfile f
                  if exists
                     then return ()
                     else touch f

withIndex :: IndexConfig 
          -> (IndexLive -> IpfsME a) 
          -> IO (Either Text a)
withIndex cfg f = withApiM (indexConfigApi cfg) $ do 
  l2$ mktree (indexConfigPath cfg)

  l2$ makeExistF (pfPath cfg)
  mc <- l2 (loadMetaCache (mcPath cfg)) >>= updateOnPF (pfPath cfg)

  l2$ makeExistF (rmPath cfg)
  rp <- l2.hdie$ loadPF (rmPath cfg)
  f (IndexLive cfg mc rp)

addToIndex :: IndexLive -> Entry -> IndexLive
addToIndex (IndexLive cfg mc rp) e = 
  let (p,m) = entryPair e
  in IndexLive cfg 
               (Map.insert p m mc) 
               (List.delete p rp)

rmFromIndex :: IndexLive -> IpfsPath -> Either Text IndexLive
rmFromIndex (IndexLive cfg mc rp) p =
  if Map.member p mc
     then Right $ IndexLive cfg 
                            (Map.delete p mc) 
                            (rp ++ [p])
     else Left "Path was not archived."


writeIndex :: IndexLive -> IO ()
writeIndex (IndexLive cfg mc rp) = do
  writePF (Map.keys mc) (pfPath cfg)
  writePF rp (rmPath cfg)
  writeMetaCache (mcPath cfg) mc

type MetaCache = Map IpfsPath Metadata

-- | Given a 'MetaCache' and a 'FilePath' pointing to a pinfile,
-- remove all cache entries not in the pinfile and build new cache
-- entries for un-cached entries in the pinfile.
updateOnPF :: FilePath -> MetaCache -> IpfsME MetaCache
updateOnPF p mc = do
  mps <- l2$ loadPF p
  case mps of
    Right ps -> do 
      let mc' = Map.restrictKeys mc (Set.fromList ps)
      foldM addMetaCache mc' ps
    Left e -> throwError e

addMetaCache :: MetaCache -> IpfsPath -> IpfsME MetaCache
addMetaCache indx p  = 
  if Map.member p indx
     then return indx
     else do 
       md <- getMeta p
       return $ Map.insert p md indx

-- | Create an index from a pinfile.  The metadata file of each ipfs
-- path in the pinfile will be parsed to create the metadata entries.
genFromPF :: FilePath -> IpfsME MetaCache
genFromPF f = updateOnPF f mempty

-- | Load an index from a file, returning an empty index if anything
-- goes wrong (this behavior is consistent with the purpose of the
-- index; all information in it is just cached from the IPFS store and
-- can be re-read).
loadMetaCache :: FilePath -> IO MetaCache
loadMetaCache p = do 
  mi <- decodeFileEither (Text.unpack . format fp $ p)
  case mi of
    Right indx -> return indx
    Left _ -> return mempty

-- | Append the new parts of the post-index to the file (ignoring
-- everything in the pre-index).
-- 
-- (Not really implemented, currently just does a 'writeMetaCache')
updateMetaCache :: FilePath 
            -> (MetaCache,MetaCache) -- ^ (Pre,Post) index
            -> IO ()
updateMetaCache p (pre,post) = writeMetaCache p post

-- | Write an index to file, overwriting whatever was there before
writeMetaCache :: FilePath -> MetaCache -> IO ()
writeMetaCache p indx = encodeFile (Text.unpack . format fp $ p) indx
