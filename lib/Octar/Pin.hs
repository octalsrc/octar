{-# LANGUAGE LambdaCase #-}

module Octar.Pin where

import Data.Text (Text)

import Turtle.Ipfs

pinAll :: Text -> [IpfsPath] -> IO ()
pinAll api paths = withApi' api (pin paths) >>= \case
  Right _ -> return ()
  Left e -> putStrLn $ "Pin failed: " <> show e

unpinAll :: Text -> [IpfsPath] -> IO ()
unpinAll api paths = withApi' api (unpin paths) >>= \case
  Right _ -> return ()
  Left e -> putStrLn $ "Unpin failed: " <> show e
