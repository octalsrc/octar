{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Octar.CLI.Opts
  ( ConfigCLI (configConfFile, configCommand)
  , Command (..)
  , AddConf (..)
  , chooseIndex
  , chooseIndexRm
  , RmConf (..)
  , MirrorConf (..)
  , getConfigCLI
  , loadConfigFile
  ) where

import Options.Applicative
import Data.Monoid ((<>))
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens hiding (argument)
import Data.Maybe (fromJust)

import Turtle (home,format,fp,die,need,fromText,testfile)

import Turtle.Ipfs
import Octar

version = "v0.3.1"

desc = "Manage an IPFS-based archive of files"
hd = "octar - an archive manager"

cmds :: (MethodSet m) => [m -> Mod CommandFields (Command m)]
cmds = 
  [ (mkcmd "add" Add addPs)
      "Add the file located at TARGET to the archive"
  , (mkcmd "rm" Rm (const rmPs))
      "Remove the entry with PATH from the archive"
  , (mkcmd "sync" id (const.pure$Refresh))
      "Update metacache and directory for index"
  , (mkcmd "mirror" Mirror (const mirrorPs))
      "Run a mirror" ]

mkcmd :: (MethodSet m) 
      => String 
      -> (a -> Command m) 
      -> (m -> Parser a)
      -> String
      -> (m -> Mod CommandFields (Command m))
mkcmd s f p d = \m -> command s (info (f <$> (p m <**> helper)) (progDesc d))

data ConfigCLI m = ConfigCLI 
  { configConfFile :: Maybe FilePath
  , configCommand :: Command m }
  deriving (Eq,Ord)

data Command m =
    Add (AddConf m)
  | Rm RmConf
  | Refresh
  | Mirror MirrorConf
  deriving (Eq,Ord)

data MirrorConf = MirrorConf 
  { mirrorIndexPort :: Maybe Int
  , mirrorGatewayPort :: Maybe Int } 
  deriving (Eq,Ord)

mirrorPs :: Parser MirrorConf
mirrorPs = MirrorConf 
  <$> option (readm read) 
             (value Nothing
              <> long "index"
              <> metavar "PORT")
  <*> option (readm read)
             (value Nothing
              <> long "gateway"
              <> metavar "PORT")

data AddConf m = AddConf
  { addTarget :: Text
  , addMethod :: MethodHint m
  , addDry :: Bool
  , addNoPrompt :: Bool
  , addCLIMessage :: Maybe Text
  , addIndexChoice :: Maybe Text }
  deriving (Eq,Ord)

chooseIndex :: MultiConfig -> AddConf m -> Either String (IndexConfig, StorageConfig)
chooseIndex mc ac = case Text.unpack <$> addIndexChoice ac of
  Just iname -> case mc^.indexWithStorage (indexes.at iname) of
    Just is -> Right is
    Nothing -> Left $ "Index \"" <> iname <> "\" is not configured."
  Nothing -> case mc^.indexWithStorage defaultIndex of
    Just is -> Right is
    Nothing -> Left $ "You must choose an index."

chooseIndexRm :: MultiConfig -> RmConf -> Either String (IndexConfig, StorageConfig)
chooseIndexRm mc rc = case Text.unpack <$> rmIndexChoice rc of
  Just iname -> case mc^.indexWithStorage (indexes.at iname) of
    Just is -> Right is
    Nothing -> Left $ "Index \"" <> iname <> "\" is not configured."
  Nothing -> case mc^.indexWithStorage defaultIndex of
    Just is -> Right is
    Nothing -> Left $ "You must choose an index."

-- | Parser for "add" command
addPs :: (MethodSet m) => m -> Parser (AddConf m)
addPs (m :: m) = AddConf
  <$> strArgument (metavar "TARGET" <> help hURI)
  <*> option auto (fold [value Auto
                        ,short 'g'
                        ,metavar "GETTER"
                        ,long "using"
                        ,help hmethod
                        ,completeWith names])
  <*> switch (fold [short 'd'
                   ,long "dry-run"
                   ,help "test fetching the target without touching the archive"])
  <*> switch (fold [short 'n'
                   ,long "no-prompt"
                   ,help hnoprompt])
  <*> mtext (long "message"
             <> short 'm'
             <> metavar "STR"
             <> help "Set an archive message")
  <*> option 
        (readm Text.pack)
        (fold [value Nothing
              ,short 'i'
              ,long "index"
              ,metavar "NAME"])
  where hmethod = "use GETTER to fetch the target (" ++ (show names) ++ ")"
        hURI = "the location or filepath of the target"
        hnoprompt = "fetch and store target without asking for a synopsis \
                    \or new filename (a \"to be refiled\" temp. synopsis \
                    \will be used)"
        names = Map.keys (Map.mapKeys Text.unpack (namesMap :: Map Text (Method m)))


mtext :: Mod OptionFields Text -> Parser (Maybe Text)
mtext ms = 
  (Just <$> strOption ms) <|> pure Nothing

e2s :: (Text -> Either Text a) -> (String -> Either String a)
e2s f s = case f (Text.pack s) of
            Right a -> Right a
            Left t -> Left (Text.unpack t)

data RmConf = RmConf
  { rmTarget :: IpfsPath
  , rmIndexChoice :: Maybe Text }
  deriving (Eq,Ord)

rmPs :: Parser RmConf
rmPs = RmConf 
  <$> argument (eitherReader (e2s mkIpfsPath)) (metavar "PATH")
  <*> option 
        (readm Text.pack)
        (fold [value Nothing
              ,short 'i'
              ,long "index"
              ,metavar "NAME"])

readm :: (String -> a) -> ReadM (Maybe a)
readm f = maybeReader (Just . Just . f)

ccPs :: (MethodSet m) => m -> Parser (ConfigCLI m)
ccPs (m :: m) = ConfigCLI
  <$> option 
        (readm (id)) 
        (fold [value Nothing
              ,short 'c'
              ,long "config"
              ,metavar "PATH"])
  <*> subparser (fold (map ($m) cmds))

systemOctarConf :: IO (Maybe FilePath)
systemOctarConf = do
  let p = "/etc/octar"
  testfile (fromText p) >>= \case
    True -> return.Just $ Text.unpack p
    False -> return Nothing

localOctarConf :: IO (Maybe String)
localOctarConf = do
  p <- format fp . (<> fromText ".octar") <$> home
  testfile (fromText p) >>= \case
    True -> return.Just $ Text.unpack p
    False -> return Nothing

chooseConfigFile :: (MethodSet m) => ConfigCLI m -> IO FilePath
chooseConfigFile ccli = case configConfFile ccli of
  Just fp -> return fp
  Nothing -> need "OCTAR_CONFIG" >>= \case
    Just t -> return $ Text.unpack t
    Nothing -> localOctarConf >>= \case
      Just p -> return p
      Nothing -> systemOctarConf >>= \case
        Just p -> return p
        Nothing -> die "Could not find any index configuration file."

loadConfigFile :: (MethodSet m) => ConfigCLI m -> IO MultiConfig
loadConfigFile ccli = do
  emc <- loadMultiConfig =<< chooseConfigFile ccli
  case emc of
    Right mc -> return mc
    Left e -> die (Text.pack e)

getConfigCLI :: (MethodSet m) => m -> Text -> IO (ConfigCLI m)
getConfigCLI (m :: m) version = 
  execParser 
    (info (ccPs m <**> versionOp (Text.unpack version) <**> helper) 
          (fullDesc <> progDesc desc <> header hd) :: ParserInfo (ConfigCLI m))

versionOp version = infoOption 
                      version
                      (long "version" <> help "Show version info")
