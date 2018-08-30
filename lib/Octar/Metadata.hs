{-# LANGUAGE OverloadedStrings #-}

module Octar.Metadata 
  ( MetaFrame (..)
  , Metadata (..)
  , mfMain
  , mfMessage
  , Timestamp (..)
  , SourceRecord (..)
  , stampNow
  , mkSR
  , mkMD
  ) where

import Prelude hiding (FilePath)

import Data.Yaml
import Data.Aeson (pairs,foldable)
import Data.Text (Text,pack,unpack)
import qualified Data.Text as Text
import qualified Data.Time as DT
import Turtle hiding (cat)

import Turtle.Ipfs

newtype Timestamp = Timestamp { tsDate :: DT.UTCTime }
                  deriving (Eq,Ord)

instance Show Timestamp where
  show = fmt


fmt :: Timestamp -> String
fmt = DT.formatTime 
        DT.defaultTimeLocale 
        (DT.iso8601DateFormat (Just "%H:%M:%S"))
      . tsDate

data SourceRecord = SourceRecord { timestamp :: Timestamp
                                 , sourceUrl :: Text }
                    deriving (Show,Eq,Ord)

instance FromJSON Timestamp where
  parseJSON = withText "Timestamp" $ \t -> 
    case DT.parseTimeM False DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (Text.unpack t) of
      Just d -> return (Timestamp d)
      Nothing -> fail "Couldn't parse timestamp."

instance FromJSON SourceRecord where
  parseJSON = withObject "SourceRecord" $ \v -> SourceRecord
    <$> v .: "date"
    <*> v .: "uri"

instance ToJSON SourceRecord where
  toJSON (SourceRecord t u) = 
    object ["uri" .= u, "date" .= fmt t]


-- | 'MetaFrame' is a monoid.  When combining two frames, the synopsis
-- values are added together (with a blank line in between), the
-- sources are added together, and the entry-point path of the second
-- replaces the first.
data MetaFrame = MetaFrame { synopsis :: Text
                           , sources :: [SourceRecord]
                           , entryPoint :: Maybe FilePath }
                 deriving (Show,Eq,Ord)

mfMain :: FilePath -> MetaFrame
mfMain f = MetaFrame mempty mempty (Just f)

mfMessage :: Text -> MetaFrame
mfMessage t = MetaFrame t mempty mempty

instance Monoid MetaFrame where
  mempty = MetaFrame mempty mempty Nothing
  mappend (MetaFrame s1 rs1 ep1) (MetaFrame s2 rs2 ep2) = 
    MetaFrame (sp s1 s2) (rs1 ++ rs2) ep
    where ep = case ep2 of
                 Nothing -> ep1
                 _ -> ep2
          sp s1 s2 = if not (Text.length s1 == 0 
                             || Text.length s2 == 0)
                        then Text.intercalate "\n\n" [s1,s2]
                        else s1 <> s2

data Metadata = Metadata { metaMain :: MetaFrame
                         , date :: Timestamp
                         , archivist :: Text }
                deriving (Show,Eq,Ord)

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \v -> do
    s <- v .: "message"
    rs <- v .:? "sources" .!= []
    d <- v .: "date"
    ep <- v .:? "main"
    a <- v .: "archivist"
    return $ Metadata (MetaFrame s rs (fromText <$> ep)) d a

instance ToJSON Metadata where
  toJSON (Metadata (MetaFrame syn src ep) date a) = 
    object $ ["message" .= syn
             ,"date" .= fmt date
             ,"archivist" .= a] ++ m ++ ss
    where m = case ep of
                Just p -> ["main" .= format fp p]
                Nothing -> []
          ss = case src of
                 [] -> []
                 _ -> ["sources" .= array (map toJSON src)]

-- | Get a 'Timestamp' for the current moment
stampNow :: IO Timestamp
stampNow = Timestamp <$> DT.getCurrentTime

-- | Create a 'MetaFrame' with a timestamped source record for a
-- 'Text' URI
mkSR :: Text -> IO MetaFrame
mkSR t = do sr <- SourceRecord <$> stampNow <*> pure t
            return $ MetaFrame mempty [sr] mempty

-- | Create a timestamped 'Metadata' by giving an archivist name and
-- 'MetaFrame'
mkMD :: Text -> MetaFrame -> IO Metadata
mkMD a m = Metadata m <$> stampNow <*> pure a
