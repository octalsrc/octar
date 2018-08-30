{-# LANGUAGE OverloadedStrings #-}

module Octar.Entry
  ( getMeta
  , EntryFrame (..)
  , mkEntryFrame
  , metaPath
  , itemPath
  , metaPathName
  , itemPathName
  , gcFrame
  , storeEntry
  , Entry (entryPair)
  ) where

import Prelude hiding (FilePath)
import qualified Data.Text as Text

import Data.Yaml
import Turtle hiding (cat)

import Turtle.Ipfs
import Octar.Metadata

mkEntryFrame :: IO EntryFrame
mkEntryFrame = r 0
  where r :: Int -> IO EntryFrame
        r n = do let path :: FilePath
                     path = fromText "/tmp" 
                            <> fromText (frameD n)
                 pe <- testpath path
                 if pe
                    then r (n+1)
                    else mktree path 
                         >> mktree (path <> itemPathName)
                         >> return (EntryFrame path)
        frameD n = "octar-entry-frame_" <> (Text.pack . show $ n)

getMeta :: IpfsPath -> IpfsME Metadata
getMeta p = do 
  r <- decodeEither' <$> cat p metaPathName
  case r of
    Right m -> return m
    Left e -> throwError (Text.pack . show $ e :: Text)

newtype EntryFrame = EntryFrame { efPath :: FilePath }

itemPath :: EntryFrame -> FilePath
itemPath (EntryFrame f) = f <> itemPathName

metaPath :: EntryFrame -> FilePath
metaPath (EntryFrame f) = f <> metaPathName

metaPathName :: FilePath
metaPathName = fromText "meta"

itemPathName :: FilePath
itemPathName = fromText "item"

-- | Clean up (delete) an entry frame
gcFrame :: EntryFrame -> IO ()
gcFrame e = rmtree (efPath e)

-- | Store an entry frame with metadata in IPFS, making it an 'Entry'.
storeEntry :: Metadata -> EntryFrame -> IpfsME Entry
storeEntry m e = do
  l2$ encodeFile (Text.unpack (format fp (metaPath e))) m
  p <- addf (efPath e)
  l2$ gcFrame e
  return (Entry (p,m))

-- | An item stored in the archive.
newtype Entry = Entry { entryPair :: (IpfsPath,Metadata) }
