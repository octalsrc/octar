{-# LANGUAGE OverloadedStrings #-}

module Octar.CLI 
  ( octarStock
  , octarCLI
  , octarCLI'
  , module Octar
  ) where

import Prelude hiding (FilePath)

import qualified System.IO as IO (hFlush,stdout)
import Turtle
import Turtle.Prelude (cd,ls,mv,env)
import Control.Foldl (list)
import Control.Monad.Except
import Data.Text (Text,pack,unpack,stripEnd)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map

import Octar
import Turtle.Ipfs
import Octar.CLI.Opts

version = "0.3.1"

refileSynopsis = "(To be refiled.)"

fetchByConf :: (MethodSet m) => AddConf m -> EntryFrame -> IO (Either Text MetaFrame)
fetchByConf (AddConf trg mth _ noprompt _) ef = 
  fetch mth (AddInfo ef noprompt trg)

orDie :: (MonadIO m) => m (Either Text a) -> m a
orDie f = do ma <- f
             case ma of
               Right a -> return a
               Left t -> liftIO $ die t

mmconcat :: (Monad m, Monoid a) => [m a] -> m a
mmconcat (a:as) = a >>= (\a -> (a <>) <$> mmconcat as)
mmconcat [] = return mempty

-- | Octar command-line tool using basic methods ("cp" and "wget")
octarStock :: IO ()
octarStock = mkOctarCLI ("octar v" <> version) CommonMethods

-- | An octar command-line tool using the given 'MethodSet'.
octarCLI :: (MethodSet m) => m -> IO ()
octarCLI = mkOctarCLI ("octar v" <> version <> " (custom)")

-- | An octar command-line tool using the given 'MethodSet', for which
-- you can provide a special version description.
octarCLI' :: (MethodSet m) => Text -> m -> IO ()
octarCLI' vs = mkOctarCLI (vs <> " (using base octar v" <> version <> ")")

mkOctarCLI :: (MethodSet m) => Text -> m -> IO ()
mkOctarCLI version methodset = orDie $ do
  conf <- getConfigCLI methodset version
  (indxc,beh) <- completeConfig conf
  case configCommand conf of

    Add c -> withIndex indxc $ \indx -> do
      l2$ runPull' beh indxc
      ef <- l2$ mkEntryFrame
      let ms = [case (addNoPrompt c, addCLIMessage c) of
                  (_,Just m) -> return . mkSynopsis $ m
                  (True,_) -> return . mkSynopsis $ refileSynopsis
                  _ -> askSynopsisEdit
               ,orDie$ fetchByConf c ef]
      md <- l2$ mkMD (indexArchivistName indxc) =<< mmconcat ms
      if addDry c
         then die "Dry run, not writing to index."
         else return ()
      l2$ putStr "Storing entry...  " >> IO.hFlush IO.stdout
      entry <- storeEntry md ef
      l2$ writeWithDirs (addToIndex indx entry)
      l2$ putStrLn "Done." >> IO.hFlush IO.stdout
      isGit <- testdir (indexConfigPath indxc <> fromText ".git")
      if isGit
         then do l2$ gitCommit (indexConfigPath indxc) "Add entry"
                 l2$ putStrLn "Git-commited entry."
         else return ()
      if addDry c
         then return ()
         else l2$ runPush' beh indxc >> return ()

    Rm p -> withIndex indxc $ \indx -> do
      l2$ runPull' beh indxc
      case rmFromIndex indx p of
        Right indx' -> do 
          l2$ writeIndex indx' >> writeWithDirs indx' 
          l2$ gitCommit (indexConfigPath indxc) "Remove entry"
        Left e -> die e
      l2$ runPush' beh indxc
      return ()

    Pin -> withApiM (indexConfigApi indxc) $ do
      l2$ runPull' beh indxc
      pin =<< (l2.orDie$ loadPF (pfPath indxc))

    Push -> runPush indxc

    Pull -> runPull indxc

    Refresh -> withIndex indxc $ \indx -> do
      l2$ runPull' beh indxc
      l2$ writeWithDirs indx

    Browse (BrowseConf muri mapi mgate mpath) -> withTmpDir $ \tmp -> do
      let tmpFile = tmp <> fromText "asdf.org"
      mc <- case muri of
              Just uri -> withGitRepo uri $ \repo -> do
                let iconf = IndexConfig repo mapi [] "browser"
                orDie$ withIndex iconf (return . liveMetaCache)
              Nothing -> 
                orDie$ withIndex indxc (return . liveMetaCache) 
      let gateway = case mgate of
                      Just gate -> gate
                      Nothing -> "https://ipfs.io"
      case mpath of
        Just f -> writeOrgDir gateway f mc
        Nothing -> do 
          let tmpFile = tmp <> fromText "asdf.org"
          writeOrgDir gateway tmpFile mc
          edit tmpFile
      return (Right ())

runPush' beh = 
  if not $ behaviorNoSync beh
     then runPush
     else const $ return (Right ())

runPush indxc = runGitCmd (indexConfigPath indxc) ["push"] 
                >> return (Right ())

runPull' beh = 
  if not $ behaviorNoSync beh
     then runPull
     else const $ return (Right ())

runPull indxc = runGitCmd (indexConfigPath indxc) ["pull"] 
                >> return (Right ())

edit :: FilePath -> IO ()
edit f = do
  med <- need "EDITOR"
  case med of
    Just ed -> proc ed [format fp f] empty >> return ()
    Nothing -> die "EDITOR variable was not set."

withGitRepo :: Text -> (FilePath -> IO a) -> IO a
withGitRepo uri action = withTmpDir $ \repoDir -> do
  runGitCmd repoDir ["clone",uri,"repo"]
  home >>= cd
  action $ repoDir <> fromText "repo"


withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = withTmpDir' "octar-thing"

withTmpDir' :: Text -> (FilePath -> IO a) -> IO a
withTmpDir' name action = do
  tmpDir <- freshTmpDir name
  result <- action tmpDir
  rmtree tmpDir
  return result

mkSynopsis :: Text -> MetaFrame
mkSynopsis t = MetaFrame t mempty mempty

freshTmp :: Text -> IO FilePath
freshTmp = freshTmp' touch

freshTmpDir :: Text -> IO FilePath
freshTmpDir = freshTmp' mktree

freshTmp' :: (FilePath -> IO ()) -> Text -> IO FilePath
freshTmp' kind t = r t 0
  where r t n = do i <- testpath (fn t n)
                   if i
                      then r t (n + 1)
                      else kind (fn t n) >> return (fn t n)
        fn t n = fromText "/tmp" 
                 <> fromText (t <> "_" <> (Text.pack . show $ n))

askSynopsisEdit :: IO MetaFrame
askSynopsisEdit = do
  f <- freshTmp "octar-entry-message"
  writeTextFile f editTemplate
  med <- need "EDITOR"
  case med of
    Just ed -> do
      ec <- proc ed [format fp f] empty
      case ec of
        ExitSuccess -> do
          message <- readTextFile f
          rm f
          return (mfMessage 
                  . (<> "\n") 
                  . (Text.dropWhileEnd (== '\n')) 
                  . dropComments
                  $ message)
        ExitFailure _ -> die "Message edit canceled."
    Nothing -> askSynopsis

dropComments :: Text -> Text
dropComments = 
  Text.unlines
  . filter (not . Text.isPrefixOf "#")
  . Text.lines

editTemplate :: Text
editTemplate = 
  "\n\
  \\n\
  \# Please write an archive message, describing what the\n\
  \# item is or why it has been stored.\n\
  \#\n\
  \# Lines starting with '#' will be ignored.\n"

askSynopsis :: IO MetaFrame
askSynopsis = MetaFrame
  <$> (Text.pack 
       <$> (putStr "Item synopsis: " 
             >> IO.hFlush IO.stdout 
             >> getLine))
  <*> (pure mempty)
  <*> (pure mempty)

gitCommit :: Turtle.FilePath -> Text -> IO ()
gitCommit homedir msg = do 
  cd homedir
  stdout (inproc "git" ["add","-u"] empty)
  stdout (inproc "git" ["commit","-m",msg,"-q"] empty)

runGitCmd :: Turtle.FilePath -> [Text] -> IO ()
runGitCmd homedir args = do 
  cd homedir
  stdout (inproc "git" (args++["-q"]) empty)
