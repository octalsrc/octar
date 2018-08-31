{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Octar.CLI.Opts
  ( ConfigCLI (configConfFile, configIndexChoice, configCommand, configBehavior)
  , Behavior (..)
  , Command (..)
  , AddConf (..)
  , BrowseConf (..)
  , getConfigCLI
  , completeConfig
  ) where

import Prelude hiding (FilePath)

import Options.Applicative
import Data.Monoid ((<>))
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map

import Turtle (FilePath,home,format,fp,die,need,fromText)

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
  , (mkcmd "pin" id (const.pure$Pin))
      "Pin all currently archived entries"
  , (mkcmd "push" id (const.pure$Push))
      "Run \"git push\" for the octar index"
  , (mkcmd "pull" id (const.pure$Pull))
      "Run \"git pull\" for the octar index"
  , (mkcmd "refresh" id (const.pure$Refresh))
      "Update metacache and directory for index"
  , (mkcmd "browse" Browse (const browsePs))
      "Open a temporary directory for the indicated index"]

mkcmd :: (MethodSet m) 
      => String 
      -> (a -> Command m) 
      -> (m -> Parser a)
      -> String
      -> (m -> Mod CommandFields (Command m))
mkcmd s f p d = \m -> command s (info (f <$> (p m <**> helper)) (progDesc d))

data ConfigCLI m = ConfigCLI 
  { configConfFile :: Maybe FilePath
  , configIndexChoice :: Maybe Text
  , configBehavior :: Behavior
  , configCommand :: Command m }
  deriving (Eq,Ord)

data Behavior = Behavior
  { behaviorNoSync :: Bool }
  deriving (Eq,Ord)

data Command m =
    Add (AddConf m)
  | Rm IpfsPath
  | Pin
  | Push
  | Pull
  | Refresh
  | Browse BrowseConf
  deriving (Eq,Ord)

data AddConf m = AddConf
  { addTarget :: Text
  , addMethod :: MethodHint m
  , addDry :: Bool
  , addNoPrompt :: Bool
  , addCLIMessage :: Maybe Text }
  deriving (Eq,Ord)

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
  where hmethod = "use GETTER to fetch the target (" ++ (show names) ++ ")"
        hURI = "the location or filepath of the target"
        hnoprompt = "fetch and store target without asking for a synopsis \
                    \or new filename (a \"to be refiled\" temp. synopsis \
                    \will be used)"
        names = Map.keys (Map.mapKeys Text.unpack (namesMap :: Map Text (Method m)))

data BrowseConf = BrowseConf 
  { browseURI :: Maybe Text
  , browseAPI :: Maybe Text
  , browseGateway :: Maybe Text
  , browseOutpath :: Maybe FilePath }
  deriving (Eq,Ord)

browsePs :: Parser BrowseConf
browsePs = BrowseConf
  <$> mtext (long "git-uri" 
             <> metavar "URI" 
             <> help "Clonable git repo of remote index")
  <*> mtext (long "api" 
             <> metavar "API" 
             <> help "Custom IPFS API to use")
  <*> mtext (long "gateway"
             <> short 'g'
             <> metavar "URI"
             <> help "IPFS gateway to use in directory links")
  <*> (fmap fromText 
       <$> mtext (short 'o'
                  <> metavar "PATH"
                  <> help "Write the directory to file instead \
                          \of opening in $EDITOR"))


mtext :: Mod OptionFields Text -> Parser (Maybe Text)
mtext ms = 
  (Just <$> strOption ms) <|> pure Nothing

e2s :: (Text -> Either Text a) -> (String -> Either String a)
e2s f s = case f (Text.pack s) of
            Right a -> Right a
            Left t -> Left (Text.unpack t)

rmPs :: Parser IpfsPath
rmPs = argument (eitherReader (e2s mkIpfsPath)) (metavar "PATH")

readm :: (String -> a) -> ReadM (Maybe a)
readm f = maybeReader (Just . Just . f)

ccPs :: (MethodSet m) => m -> Parser (ConfigCLI m)
ccPs (m :: m) = ConfigCLI
  <$> option 
        (readm (fromText . Text.pack)) 
        (fold [value Nothing
              ,short 'c'
              ,long "config"
              ,metavar "PATH"])
  <*> option 
        (readm Text.pack)
        (fold [value Nothing
              ,short 'i'
              ,long "index"
              ,metavar "NAME"])
  <*> (Behavior <$> switch (short 'x' 
                            <> long "no-sync" 
                            <> help "Do not automatically pull or push the index"))
  <*> subparser (fold (map ($m) cmds))

completeConfig :: (MethodSet m) => ConfigCLI m -> IO (IndexConfig,Behavior)
completeConfig (ConfigCLI mf mt beh comm) = do
  indexes <- case mf of
    Just f -> readConfFile f
    Nothing -> do
      mf <- need "OCTAR_CONFIG"
      case mf of
        Just f -> readConfFile (fromText f)
        Nothing -> home >>= (\h -> readConfFile (h <> fromText ".octar"))
  case selectIndex mt indexes of
    Right i -> return (i,beh)
    Left e -> die e
  where readConfFile f = do 
          r <- decodeFileEither (Text.unpack (format fp f))
          case r of
            Right c -> return c
            Left e -> do print e
                         die "Couldn't read config file."

getConfigCLI :: (MethodSet m) => m -> Text -> IO (ConfigCLI m)
getConfigCLI (m :: m) version = 
  execParser 
    (info (ccPs m <**> versionOp (Text.unpack version) <**> helper) 
          (fullDesc <> progDesc desc <> header hd) :: ParserInfo (ConfigCLI m))

versionOp version = infoOption 
                      version
                      (long "version" <> help "Show version info")
