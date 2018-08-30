{-# LANGUAGE OverloadedStrings #-}

module Octar.Method.Common
  ( CommonMethods (CommonMethods)
  , module Octar.Method
  ) where

import Prelude hiding (FilePath)
import Turtle
import Data.Text (Text,pack,unpack,stripEnd,isPrefixOf)
import Data.Map (Map,(!))
import qualified Data.Map as Map

import Octar.Metadata
import Octar.Method
import Octar.Entry


data CommonMethods = CommonMethods

instance MethodSet CommonMethods where
  namesMap = Map.fromList
    [("wget", mtf (\(AddInfo e noPrompt t) -> do 
        cdItem e
        stdout (inproc "wget" [t] empty)
        md1 <- if noPrompt
                  then identSingle e
                  else renameSingle e
        (Right . (md1 <>)) <$> mkSR t))
    ,("cp", mtf (\(AddInfo e noPrompt t) -> do
        procOrDie "cp" ["-vr",t,format fp (itemPath e)] empty
        Right <$> if noPrompt
                      then identSingle e
                      else renameSingle e))]

  chooseAuto t 
    | t <:: "https://" = sel "wget"
    | t <:: "http://" = sel "wget"
    | otherwise = sel "cp"
    where sel k = Just $ namesMap ! k

-- | Check if first argument has second as prefix
(<::) :: Text -> Text -> Bool
(<::) t p = p `isPrefixOf` t


procOrDie :: Text -> [Text] -> Shell Line -> IO ()
procOrDie c as inp = do
  r <- proc c as inp
  case r of
    ExitSuccess -> return ()
    ExitFailure e -> print e >> die "Process failed."
