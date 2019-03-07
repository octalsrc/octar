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
import Control.Lens
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import System.Posix.Signals

import Octar
import Octar.Discard
import Octar.Gateway
import Turtle.Ipfs
import Turtle.Git (sOrDie)
import qualified Turtle.Git as Git
import Octar.CLI.Opts
import Octar.Index.Frontend.StaticWeb
import Octar.Index (MetaCache)

import Network.Discard (defaultDManagerSettings, onValUpdate)
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
  mc <- loadConfigFile conf :: IO MultiConfig
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
        ref <- withApi' (Text.pack $ s^.storageApiMultiAddr) (storeEntry md efr) >>= \case
          Right ent -> return . fst . entryPair $ ent
          Left e -> die e
        putStrLn "Done."

        runIndexNodeAwait (i,s) $ \_ cc -> do
          carol cc $ issue (ef$ RGAppend ref)
          RGArray es <- carol cc queryT :: IO (RGArray IpfsPath)
          mapM_ print es
        return (Right ())

      Left e -> die (Text.pack e)

    Rm c -> case chooseIndexRm mc c of
      Right (i,s) -> do
        let ref = rmTarget c
            script i man = do
              RGArray ps <- carol man queryT
              if ref `elem` ps
                 then carol man $ issue (ef$ RGRemove ref)
                 else die "That ref isn't in the index."
        runIndexNodeAwait (i,s) script
        return (Right ())
      Left e -> die (Text.pack e)

    -- The mirror serves /all/ indexes at the same time, with a
    -- separate discard node for each
    Mirror c -> do serverInfo <- case (mirrorIndexPort c, mirrorGatewayPort c) of
                     (Just ip, Just gp) -> return $ Just (ip,gp)
                     (Nothing,Nothing) -> return $ Nothing
                     _ -> die "Must have both index and gateway, or neither."
                   endv <- newTVarIO False
                   installHandler 
                     keyboardSignal 
                     (Catch $ atomically (swapTVar endv True) >> return ()) 
                     Nothing
                   (ml,endedVs) <- buildLive (launchNode endv) mc
                   let server req resp = case map Text.unpack (pathInfo req) of
                         [iname] -> case ml^.liveCache.at iname of
                           Just mtcv -> do 
                             mtc <- readTVarIO mtcv
                             let gw = "http://localhost:" 
                                      <> (case serverInfo of
                                            Just (_,gp) -> show gp)
                                      <> "/" <> (mc^.indexes.at iname._Just.indexStorageName)
                             resp $ responseLBS 
                                      status200 
                                      [("Content-Type", "text/html")]
                                      (indexWebpage' gw mtc)
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
                   case serverInfo of
                     Just (ip,gp) -> do forkIO $ octarGateway gp ml
                                        forkIO $ Warp.run ip server
                                        return ()
                     Nothing -> return ()
                   waitForExits endedVs
                   return (Right ())

      where iss :: [(String,(IndexConfig,StorageConfig))]
            iss = map (\iname -> (iname, fromJust $ mc^.indexWithStorage (indexes.at iname))) 
                      (Map.keys (mc^.indexes))

            -- This should just stop until all launched nodes have
            -- signalled their exits.  The launched nodes should wait
            -- on the Ctrl-C signal and then shut down.
            waitForExits :: [TVar Bool] -> IO ()
            waitForExits = atomically . mapM_ (\tv -> check =<< readTVar tv)

launchNode :: TVar Bool -> String -> MultiConfig -> IO (TVar MetaCache, TVar Bool) 
launchNode endv iname mc = do
  nV <- newTVarIO False
  mcV <- newTVarIO mempty :: IO (TVar MetaCache)
  let (i,s) = fromJust $ mc^.indexWithStorage (indexes.at iname)
      script _ man = do onUp =<< carol man queryT
                        atomically $ check =<< readTVar endv
      api = pack (s^.storageApiMultiAddr)
      onUp s = do 
        mc <- readTVarIO mcV
        Right (mc',_,_) <- withApi' api (updateMC mc s)
        atomically $ swapTVar mcV mc'
        return ()
      settings = defaultDManagerSettings { onValUpdate = onUp }
  forkIO $ do runIndexNode' (i,s) settings script
              atomically $ swapTVar nV True
              return ()
  return (mcV,nV)

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
