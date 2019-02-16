{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import Control.Lens
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import System.Posix.Signals

import Octar
import Octar.Discard
import Turtle.Ipfs
import Turtle.Git (sOrDie)
import qualified Turtle.Git as Git
import Octar.CLI.Opts
import Octar.Index.Frontend.StaticWeb
import Octar.Index (MetaCache)

import Network.Discard
import Lang.Carol hiding (Add)

import Network.Wai
import Network.HTTP.Types (status200,status404)
import qualified Network.Wai.Handler.Warp as Warp (run)

version = "0.4.0"

refileSynopsis = "(To be refiled)"

fetchByConf :: (MethodSet m) => AddConf m -> EntryFrame -> IO (Either Text MetaFrame)
fetchByConf (AddConf trg mth _ noprompt _ _) ef = 
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
  mc <- loadConfigFile conf
  case configCommand conf of
  
    Add c -> case chooseIndex mc c of
      Right (i,s) -> do 
        efr <- mkEntryFrame
        let ms = [case (addNoPrompt c, addCLIMessage c) of
                    (_,Just m) -> return . mkSynopsis $ m
                    (True,_) -> return . mkSynopsis $ refileSynopsis
                    _ -> askSynopsisEdit
                 ,orDie$ fetchByConf c efr]
        md <- mkMD (Text.pack $ i^.indexArchivist) =<< mmconcat ms
        if addDry c
           then die "Dry run, not writing to index."
           else return ()
        putStr "Storing entry...  " >> IO.hFlush IO.stdout
        ref <- withApiM (Text.pack <$> s^.storageApi) (storeEntry md efr) >>= \case
          Right ent -> return . fst . entryPair $ ent
          Left e -> die e
        putStrLn "Done."

        (settings,await) <- awaitNetwork defaultDManagerSettings (Just 1000000)
        let script i man = do
              await
              runCarolR man $ issue (ef$ RGAppend ref)
              RGArray es <- runCarolR man $ query crT
              mapM_ print es

        runIndexNode (i,s) settings script
        return (Right ())

      Left e -> die (Text.pack e)

    Rm c -> case chooseIndexRm mc c of
      Right (i,s) -> do
        (settings,await) <- awaitNetwork defaultDManagerSettings (Just 1000000)
        let ref = rmTarget c
            script i man = do
              await
              RGArray ps <- runCarolR man $ query crT
              if ref `elem` ps
                 then runCarolR man $ issue (ef$ RGRemove ref)
                 else die "That ref isn't in the index."
        runIndexNode (i,s) settings script
        return (Right ())
      Left e -> die (Text.pack e)

    -- The mirror serves /all/ indexes at the same time, with a
    -- separate discard node for each
    Mirror c -> do endv <- newTVarIO False
                   installHandler 
                     keyboardSignal 
                     (Catch $ atomically (swapTVar endv True) >> return ()) 
                     Nothing
                   tvs <- mapM (launchNode endv) iss
                   let mcMap = Map.fromList (map snd tvs) :: Map String (StorageConfig, TVar MetaCache)
                       server req resp = case pathInfo req of
                         [iname] -> case Map.lookup (Text.unpack iname) mcMap of
                           Just (s,mcv) -> do 
                             mc <- readTVarIO mcv
                             let gw = case s^.storageGateway of
                                        Just gw -> gw
                                        Nothing -> "https://gateway.ipfs.io/ipfs"
                             resp $ responseLBS 
                                      status200 
                                      [("Content-Type", "text/html")]
                                      (indexWebpage' gw mc)
                           Nothing -> resp $ responseLBS 
                                               status404 
                                               [("Content-Type","text/plain")]
                                               "No index by that name exists."
                         [] -> resp $ responseLBS 
                                        status200 
                                        [("Content-Type","text/html")]
                                        (mainPage (Map.keys (mc^.indexes)))
                         _ -> resp $ responseLBS 
                                       status404 
                                       [("Content-Type","text/plain")]
                                       "Try an index."
                   forkIO $ Warp.run 3000 server
                   waitForExits (map fst tvs)
                   return (Right ())

      where iss :: [(String,(IndexConfig,StorageConfig))]
            iss = map (\iname -> (iname, fromJust $ mc^.indexWithStorage (indexes.at iname))) 
                      (Map.keys (mc^.indexes))

            launchNode :: TVar Bool 
                       -> (String, (IndexConfig, StorageConfig))
                       -> IO (TVar Bool, (String, (StorageConfig, TVar MetaCache)))
            launchNode endv (iname,(i,s)) = do
              -- The script just waits until the end-signal is true
              nV <- newTVarIO False
              mcV <- newTVarIO mempty
              let script _ man = do onUp =<< runCarolR man (query crT)
                                    atomically $ check =<< readTVar endv
                  api = fmap pack (s^.storageApi)
                  onUp s = do 
                    mc <- readTVarIO mcV
                    Right (mc',_,_) <- withApiM api (updateMC mc s)
                    atomically $ swapTVar mcV mc'
                    return ()
                  settings = defaultDManagerSettings { onStoreUpdate = onUp.fst }
              -- The new thread runs the discard node (which exits
              -- when endv goes to true) and then announces its exit
              -- by setting its thread-specific nV to true
              forkIO $ do runIndexNode (i,s) settings script
                          atomically $ swapTVar nV True
                          return ()
              return (nV,(iname, (s,mcV)))

            -- This should just stop until all launched nodes have
            -- signalled their exits.  The launched nodes should wait
            -- on the Ctrl-C signal and then shut down?
            waitForExits :: [TVar Bool] -> IO ()
            waitForExits = atomically . mapM_ (\tv -> check =<< readTVar tv)

    -- Add c -> do 
    --   withIndex indxc $ \indx -> do
    --     ef <- l2$ mkEntryFrame
    --     let ms = [case (addNoPrompt c, addCLIMessage c) of
    --                 (_,Just m) -> return . mkSynopsis $ m
    --                 (True,_) -> return . mkSynopsis $ refileSynopsis
    --                 _ -> askSynopsisEdit
    --              ,orDie$ fetchByConf c ef]
    --     md <- l2$ mkMD (indexArchivistName indxc) =<< mmconcat ms
    --     if addDry c
    --        then die "Dry run, not writing to index."
    --        else return ()
    --     l2$ putStr "Storing entry...  " >> IO.hFlush IO.stdout
    --     entry <- storeEntry md ef
    --     l2$ writeWithDirs (addToIndex indx entry)
    --     l2$ putStrLn "Done." >> IO.hFlush IO.stdout
    --     isGit <- l2$ Git.isRepo (indexConfigPath indxc)
    --     if isGit
    --        then do l2.sOrDie$ Git.addU (indexConfigPath indxc)
    --                l2.sOrDie$ Git.commit "Add entry" (indexConfigPath indxc)
    --                l2$ putStrLn "Git-commited entry."
    --        else return ()
    --     if addDry c
    --        then return ()
    --        else l2$ runPush' beh indxc >> return ()

    -- Rm p -> do
    --   runPull' beh indxc
    --   withIndex indxc $ \indx -> do
    --     case rmFromIndex indx p of
    --       Right indx' -> do 
    --         l2$ writeIndex indx' >> writeWithDirs indx'
    --         l2.sOrDie$ Git.addU (indexConfigPath indxc)
    --         l2.sOrDie$ Git.commit "Remove entry" (indexConfigPath indxc)
    --       Left e -> die e
    --     l2$ runPush' beh indxc
    --     return ()

    -- Pin -> do
    --   runPull' beh indxc
    --   withApiM (indexConfigApi indxc) (pin =<< (l2.orDie$ loadPF (pfPath indxc)))

    -- Push -> runPush indxc

    -- Pull -> runPull indxc

    -- Refresh -> do
    --   runPull' beh indxc
    --   withIndex indxc (l2 . writeWithDirs)

    -- Browse (BrowseConf muri mapi mgate mpath) -> withTmpDir $ \tmp -> do
    --   let tmpFile = tmp <> fromText "asdf.org"
    --   mc <- case muri of
    --           Just uri -> withGitRepo uri $ \repo -> do
    --             let iconf = IndexConfig repo mapi [] "browser"
    --             orDie$ withIndex iconf (return . liveMetaCache)
    --           Nothing -> 
    --             orDie$ withIndex indxc (return . liveMetaCache) 
    --   let gateway = case mgate of
    --                   Just gate -> gate
    --                   Nothing -> "https://ipfs.io"
    --   case mpath of
    --     Just f -> writeOrgDir gateway f mc
    --     Nothing -> do 
    --       let tmpFile = tmp <> fromText "asdf.org"
    --       writeOrgDir gateway tmpFile mc
    --       edit tmpFile
    --   return (Right ())

-- runPush' beh = 
--   if not $ behaviorNoSync beh
--      then runPush
--      else const $ return (Right ())

-- runPush indxc = sOrDie (Git.push (indexConfigPath indxc))
--                 >> return (Right ())

-- runPull' beh = 
--   if not $ behaviorNoSync beh
--      then runPull
--      else const $ return (Right ())

-- runPull indxc = sOrDie (Git.pull (indexConfigPath indxc))
--                 >> return (Right ())

edit :: FilePath -> IO ()
edit f = do
  med <- need "EDITOR"
  case med of
    Just ed -> proc ed [format fp f] empty >> return ()
    Nothing -> die "EDITOR variable was not set."

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
