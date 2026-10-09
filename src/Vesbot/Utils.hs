{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Utils
  ( liftIO
  , forkIO
  , threadDelay
  , void
  , forM
  , forM_
  , fromBot
  , showT
  , find
  , parseJSON
  ) where

import Vesbot.Logging as Logging (echo)

import Discord.Types

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

import Control.Monad.IO.Class (liftIO)
import UnliftIO.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forM, forM_)
import Data.List (find)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS


showT :: Show a => a -> T.Text
showT = T.show


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor


parseJSON :: A.FromJSON a => FilePath -> IO (Maybe a)
parseJSON path = do
  jsonData <- BS.readFile path
  let decoded = A.decodeStrict jsonData
  case decoded of
    Nothing -> do
      Logging.echo $ "Error parsing the JSON Data in " <> T.pack path <> "."
      return Nothing
    Just d  -> return (Just d)
