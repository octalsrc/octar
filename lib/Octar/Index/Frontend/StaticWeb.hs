{-# LANGUAGE OverloadedStrings #-}

module Octar.Index.Frontend.StaticWeb
  (indexWebpage
  ,indexWebpage'
  
  ) where

import Octar.Index (MetaCache)
import Octar.Metadata
import Turtle.Ipfs
import Turtle (format,fp)
import qualified Data.Text as Text
import Data.Monoid (mconcat)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)

import Lucid

indexWebpage :: String -- ^ Storage gateway path
             -> MetaCache -- ^ Index metacache
             -> Html ()
indexWebpage gw mc = 
  html_ $ 
    (head_ (meta_ [charset_ "utf-8"])
     <> body_ (ul_ $ mconcat (map (entry gw) (Map.assocs mc))))


indexWebpage' :: String -- ^ Storage gateway path
              -> MetaCache -- ^ Index metacache
              -> ByteString
indexWebpage' gw mc = renderBS $ indexWebpage gw mc

entry :: String -- ^ Storage gateway path
      -> (IpfsPath,Metadata)
      -> Html ()
entry gw (ref,(Metadata (MetaFrame msg _ mep) _ _)) = 
  let ep = case mep of
             Just ep -> "/" <> format fp ep
             Nothing -> ""
      lnk = Text.pack gw <> "/" <> ipfsPath ref <> "/item" <> ep
      hdline = head $ Text.lines msg
  in li_ (a_ [href_ lnk] (toHtml hdline))
