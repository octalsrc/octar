{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Octar.Method
  ( MethodHint (..)
  , Method
  , AddInfo (..)
  , mtf
  , MethodSet (..)
  , choose
  , fetch
  , identSingle
  , identSingle'
  , renameSingle
  , cdItem
  ) where

import Prelude hiding (FilePath)
import qualified System.IO as IO (hFlush,stdout)
import Turtle (FilePath,(<>),fromText,fold,ls,mv,format,fp,cd,filename)
import Text.Read
import qualified Text.ParserCombinators.ReadP
import Data.Text (Text,pack)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Foldl (list)

import Octar.Metadata
import Octar.Entry

data MethodHint m = Auto | Hint Text deriving (Show,Eq,Ord)

data Method m = Method { methodFun :: AddInfo -> IO (Either Text MetaFrame) }

data AddInfo = AddInfo
  { addInfoEntryFrame :: EntryFrame
  , addInfoNoPrompt :: Bool
  , addInfoTarget :: Text }

mtf :: (AddInfo -> IO (Either Text MetaFrame)) -> Method m
mtf = Method

class MethodSet m where
  namesMap :: Map Text (Method m)
  chooseAuto :: Text -> Maybe (Method m)

choose :: (MethodSet m) => MethodHint m -> Text -> Maybe (Method m)
choose (Hint s) _ = namesMap Map.!? s
choose Auto t = chooseAuto t

instance (MethodSet m) => Read (MethodHint m) where
  readPrec = do s <- pack <$> getFullStr
                if s `Map.member` (namesMap :: Map Text (Method m))
                   then return (Hint s :: MethodHint m)
                   else pfail
    where getFullStr = (do c <- get
                           (c:) <$> step getFullStr)
                       +++
                       (do EOF <- lexP
                           return [])

-- | Get the target file from the specified URI, saving it into the
-- specified entry directory.  If successful, a (possibly empty)
-- 'MetaFrame' will be returned containing anything the method
-- learned.
--
-- The returned 'MetaFrame' may include a list of sources or an
-- entry-point 'FilePath' for the item.
fetch :: (MethodSet m) 
      => MethodHint m -- ^ Method to use
      -> AddInfo
      -> IO (Either Text MetaFrame)
fetch h i  = case choose h (addInfoTarget i) of
               Just (Method mf) -> mf i
               Nothing -> return (Left "Could not choose fetch-method.")

-- | Given a directory 'FilePath', if it contains exactly one file,
-- this function returns the file's name.
identSingle' :: FilePath -> IO (Maybe FilePath)
identSingle' f = do files <- fold (ls f) list
                    return (case files of
                              (f : []) -> Just (filename f)
                              _ -> Nothing)

identSingle :: EntryFrame -> IO MetaFrame
identSingle e = MetaFrame mempty mempty <$> identSingle' (itemPath e)

-- | Given directory 'FilePath', if it contains exactly one file, ask
-- for a new name and move it (via the CLI).
renameSingle :: EntryFrame -> IO MetaFrame
renameSingle e = do 
  mf <- identSingle' (itemPath e)
  case mf of
    Just f -> do
      TIO.putStrLn $ "File " <> format fp f <> " staged."
      TIO.putStr $ "New name for file? (blank for no change) "
      IO.hFlush IO.stdout
      newName <- TIO.getLine
      if Text.length newName /= 0
         then do 
           TIO.putStrLn $ "Changed filename to \"" <> newName <> "\"."
           let newF = fromText newName
           mv ((itemPath e) <> f) ((itemPath e) <> newF)
           return $ MetaFrame mempty mempty (Just newF)
         else do
           TIO.putStrLn $ "Keeping filename."
           return $ MetaFrame mempty mempty mf
    Nothing -> return $ MetaFrame mempty mempty Nothing

-- | @cd@ to the item staging directory for an entry
cdItem :: EntryFrame -> IO ()
cdItem = cd . itemPath
