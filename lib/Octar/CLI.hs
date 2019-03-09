{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Exception (finally)
import Control.Lens
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import System.Posix.Signals

import Octar
import Octar.Discard
import Octar.IndexServer
import Octar.GatewayServer
import Turtle.Ipfs
import Turtle.Git (sOrDie)
import qualified Turtle.Git as Git
import Octar.CLI.Opts
import Octar.Index.Frontend.StaticWeb
import Octar.Index (MetaCache,loadPF)
import Octar.Pin

import Network.Discard (defaultDManagerSettings, onValUpdate)
import Lang.Carol hiding (Add)

import Network.Wai
import Network.HTTP.Types (status200,status404)
import qualified Network.Wai.Handler.Warp as Warp (run)

version = "0.4.0"

refileSynopsis = "(To be refiled)"

fetchByConf :: (MethodSet m) => AddConf m -> EntryFrame -> IO (Either Text MetaFrame)
fetchByConf (AddConf trg mth _ noprompt _ _ _) ef = 
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
  mc <- loadConfigFile conf :: IO MultiConfig
  case configCommand conf of
  
    Add c -> case chooseIndex mc c of
      Right (i,s) -> do 
        if addRaw c
           then case mkIpfsPath (addTarget c) of
                  Right ref -> do runIndexNodeAwait 
                                   (i,s) 
                                   (\_ cc -> carol cc $ issue (ef$ RGAppend ref))
                                  return (Right ())
                  Left e -> die e
           else do efr <- mkEntryFrame
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
                   ref <- withApi' (Text.pack $ s^.storageApiMultiAddr) (storeEntry md efr) >>= \case
                     Right ent -> return . fst . entryPair $ ent
                     Left e -> die e
                   putStrLn "Done."
                   runIndexNodeAwait (i,s) $ \_ cc -> carol cc $ issue (ef$ RGAppend ref)
                   return (Right ())

      Left e -> die (Text.pack e)

    Import c -> case chooseIndexImport mc c of
      Right (i,s) -> loadPF (fromText . pack . importFile $ c) >>= \case
        Right refs -> do let e = foldl (\e ref -> e |>>| ef (RGAppend ref)) ef0 refs
                         runIndexNodeAwait (i,s) (\_ cc -> carol cc $ issue e)
                         return (Right ())
        Left e -> die e
      Left e -> die (Text.pack e)

    Ls c -> case chooseIndexLs mc c of
      Right (i,s) -> do
        RGArray es <- runIndexNodeAwait (i,s) $ \_ cc -> carol cc queryT :: IO (RGArray IpfsPath)
        mapM_ print es
        return (Right ())
      Left e -> die (pack e)

    Rm c -> case chooseIndexRm mc c of
      Right (i,s) -> do
        let ref = rmTarget c
            script i man = do
              RGArray ps <- carol man queryT
              if ref `elem` ps
                 then carol man $ issue (ef$ RGRemove ref)
                 else die "That ref is not in the index."
        runIndexNodeAwait (i,s) script
        return (Right ())
      Left e -> die (Text.pack e)

    -- The mirror serves /all/ indexes at the same time, with a
    -- separate discard node for each
    Mirror c -> do
      endv <- setupEndVar
      (ml, endedVs) <- buildLive (launchNode endv (not $ mirrorNoPin c)) mc

      -- Launch gateway server if configured
      case mirrorGatewayPort c of
        Just port -> forkIO (octarGateway port ml) >> return ()
        Nothing -> return ()

      -- Launch index server if configured
      case (mirrorIndexPort c, indexGatewayUri c) of
        (Just port, Right uri) -> do
          forkIO $ runIndexServer (iserverConf uri port) ml
          return ()
        (Just _, Left err) -> die (pack err)
        _ -> return ()

      -- Wait for indexes to exit
      waitForExits endedVs
      return $ Right ()


waitForExits :: [TVar Bool] -> IO ()
waitForExits = atomically . mapM_ (\tv -> check =<< readTVar tv)

setupEndVar :: IO (TVar Bool)
setupEndVar = do
  endv <- newTVarIO False
  installHandler 
    keyboardSignal 
    (Catch $ atomically (swapTVar endv True) >> return ()) 
    Nothing
  return endv

launchNode :: TVar Bool
           -> Bool
           -> String 
           -> MultiConfig 
           -> IO (TVar MetaCache, TVar Int, TVar Bool) 
launchNode endv pinB iname mc = do
  nV <- newTVarIO False
  mcV <- newTVarIO mempty :: IO (TVar MetaCache)
  pinV <- newTVarIO 0
  let (i,s) = fromJust $ mc^.indexWithStorage (indexes.at iname)
      script _ man = do onUp =<< carol man queryT
                        atomically $ check =<< readTVar endv
      api = pack (s^.storageApiMultiAddr)
      onUp s = do 
        -- putStrLn "Running onUp..."
        mc <- readTVarIO mcV
        Right (mc',new,rmd) <- withApi' api (updateMC mc s)
        atomically $ swapTVar mcV mc'
        if length new > 0 && pinB
           then do forkIO $ do putStrLn $ "[" <> iname <> "] " <> "Pinning " <> show (length new) <> " items..."
                               atomically $ modifyTVar pinV (+ 1)
                               pinAll api new
                               atomically $ modifyTVar pinV (\p -> p - 1)
                               putStrLn $ "[" <> iname <> "] " <>"Done."
                               return ()
                   return ()
           else return ()
        if length rmd > 0
           then forkIO (unpinAll api rmd) >> return ()
           else return ()
        return ()
      settings = defaultDManagerSettings { onValUpdate = onUp }
  forkIO $ finally (runIndexNode' (i,s) settings script >> return ()) 
                   (atomically $ swapTVar nV True)
  return (mcV,pinV,nV)

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
