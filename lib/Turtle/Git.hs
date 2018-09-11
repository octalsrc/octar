{-# LANGUAGE OverloadedStrings #-}

-- | Some useful git functions
module Turtle.Git 
  ( git
  , addU
  , commit
  , push
  , pull
  , isRepo
  , sOrDie
  ) where

import Prelude hiding (FilePath)

import Turtle
import Data.Text (Text)
import qualified Data.Text as Text

-- | Run a git CLI invocation in the specified repo directory.
--
-- >>> git ["fetch", "-a"] (fromText "./path/to/my/repo")
--
git :: [Text] -> FilePath -> IO ExitCode
git args p = proc "git" (["-C",format fp p] ++ args) empty

-- | Run @git add -u@ in the specified repo directory.
addU :: FilePath -> IO ExitCode
addU = git ["add","-u"]

-- | Run @git commit@, using the provided 'Text' commit message, in
-- the specified repo directory.
commit :: Text -> FilePath -> IO ExitCode
commit msg = git ["commit","-m",msg,"-q"]

-- | Run @git push@ in the specified repo directory.
push :: FilePath -> IO ExitCode
push = git ["push","-q"]

-- | Run @git pull@ in the specified repo directory.
pull :: FilePath -> IO ExitCode
pull = git ["pull","-q"]

-- | Check if the given directory is a git repo
isRepo :: FilePath -> IO Bool
isRepo p = testdir (p <> fromText ".git")

-- | Consume an exitcode, dying if it is a failure
sOrDie :: IO ExitCode -> IO ()
sOrDie action = 
  do res <- action
     case res of
       ExitSuccess -> return ()
       ExitFailure _ -> die "Git command failed."
