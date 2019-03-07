{-# LANGUAGE OverloadedStrings #-}

module Octar.Index.Frontend.StaticWeb
  ( indexWebpage
  , indexWebpage'
  , mainPage

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
             -> String -- ^ Storage name
             -> MetaCache -- ^ Index metacache
             -> Html ()
indexWebpage gw stor mc = 
  mkPage (ul_ $ mconcat (map (entry (gw <> "/" <> stor)) (Map.assocs mc)))

mainPage :: [String] -> ByteString
mainPage ss = renderBS (ul_ $ mconcat (map (\p -> li_ (a_ [href_ (Text.pack p)] (toHtml p))) ss))

mkPage :: Html () -> Html ()
mkPage b = 
  html_ $ 
    (head_ (meta_ [charset_ "utf-8"])
     <> body_ b)

indexWebpage' :: String -- ^ Storage gateway path
              -> String -- ^ Storage name
              -> MetaCache -- ^ Index metacache
              -> ByteString
indexWebpage' gw stor mc = renderBS $ indexWebpage gw stor mc

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
