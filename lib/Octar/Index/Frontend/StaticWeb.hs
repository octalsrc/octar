{-# LANGUAGE OverloadedStrings #-}

module Octar.Index.Frontend.StaticWeb
  ( indexWebpage
  , indexWebpage'
  , mainPage

  ) where

import Data.List (sortOn)
import Octar.Index (MetaCache)
import Octar.Metadata
import Turtle.Ipfs
import Turtle (format,fp)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Monoid (mconcat)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types (urlEncode)

import Lucid

indexWebpage :: String -- ^ Storage gateway path
             -> String -- ^ Storage name
             -> MetaCache -- ^ Index metacache
             -> Bool -- ^ Pin status
             -> Html ()
indexWebpage gw stor mc pstat = 
  mkPage $ do if pstat
                 then p_ "Pin status: ✔"
                 else p_ "Pin status: ❌"
              ul_ $ mconcat (map (entry (gw <> "/" <> stor)) entries)
  where entries = reverse $ sortOn (metaDate.snd) (Map.assocs mc)

urlEncodeT :: Text -> Text
urlEncodeT = decodeUtf8 . urlEncode False . encodeUtf8

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
              -> Bool -- ^ Pin status
              -> ByteString
indexWebpage' gw stor mc ps = renderBS $ indexWebpage gw stor mc ps

entry :: String -- ^ Storage gateway path
      -> (IpfsPath,Metadata)
      -> Html ()
entry gw (ref,(Metadata (MetaFrame msg _ mep) _ _)) = 
  let ep = case mep of
             Just ep -> "/" <> urlEncodeT (format fp ep)
             Nothing -> ""
      lnk = Text.pack gw <> "/" <> ipfsPath ref <> "/item" <> ep
      hdline = head $ Text.lines msg
  in li_ (a_ [href_ lnk] (toHtml hdline))
