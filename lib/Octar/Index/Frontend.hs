{-# LANGUAGE OverloadedStrings #-}

module Octar.Index.Frontend
  ( writeOrgDir
  , writeWithDirs

  ) where

import Prelude hiding (FilePath)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Turtle (FilePath,writeTextFile,fromText,(<>),format,fp)
import Network.HTTP.Types (urlEncode)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)

import Turtle.Ipfs
import Octar.Metadata
import Octar.Entry
import Octar.Index

data SSyn = SSyn { headline :: Text
                 , body :: Maybe Text }

mkSSyn :: Text -> SSyn
mkSSyn t = 
  case Text.lines t of
    hl:b -> 
      if Text.length hl <= 68
         then SSyn hl (mkBody b)
         else let (hl1,hl2) = Text.splitAt 67 hl
              in SSyn (hl1 <> "+") (mkBody (hl2:b))

mkBody :: [Text] -> Maybe Text
mkBody [] = Nothing
mkBody ts = Just $ Text.intercalate "\n" ts

urlEncodeT :: Text -> Text
urlEncodeT = decodeUtf8 . urlEncode True . encodeUtf8

-- | Create a fancy Org-Mode directory file, in which each archive
-- entry is an outline header that can be expanded to see the full
-- entry synopsis.
writeOrgDir :: Text -- ^ IPFS gateway for links (e.g. "@https://ipfs.io@")
            -> FilePath -- ^ Where to write the index
            -> MetaCache
            -> IO ()
writeOrgDir h p indx = writeTextFile p (Text.concat $ map wr entries)
  where entries = List.sortBy
          (\(_,Metadata _ ts1 _) (_,Metadata _ ts2 _) -> compare ts2 ts1)
          (Map.assocs indx)
        wr (p,Metadata (MetaFrame s rs ep) _ _) = 
          let (SSyn hd b) = mkSSyn s
          in ("* " 
              <> (lnk hd p ep)
              <> case b of
                   Just b -> "\n" <> b <> "\n"
                   Nothing -> mempty
              <> case rs of
                   [] -> mempty
                   _ -> "\n" <> Text.intercalate "\n" (map (("- " <>) . sourceUrl) rs))
              <> "\n"
        lnk hd p ep = "[[" <> h <> fIpfsPath p <> "/item" <> mep <> "][" <> hd <> "]]"
          where mep = case ep of
                        Just f -> "/" <> urlEncodeT (format fp f)
                        Nothing -> ""

writeWithDirs :: IndexLive -> IO ()
writeWithDirs indx = do 
  writeIndex indx
  mapM_ w (indexConfigDirs (liveConfig indx))
  where w (DirConfig p g) = writeOrgDir g p (liveMetaCache indx)
