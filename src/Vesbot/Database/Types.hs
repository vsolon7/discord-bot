{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Database.Types
  ( DBConnection
  , withDB
  ) where

import Discord.Internal.Types.Prelude (DiscordId)

import Control.Concurrent.MVar (MVar, withMVar, newMVar)
import Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Ok
import Text.Read (readMaybe)
import Data.Typeable
import qualified Data.Text as T

type DBConnection = MVar SQL.Connection

withDB :: DBConnection -> (SQL.Connection -> IO a) -> IO a
withDB = withMVar


instance Typeable a => FromField (DiscordId a) where
  fromField f = do
    s <- fromField f :: Ok T.Text
    case readMaybe (T.unpack s) of
      Just i -> Ok i
      Nothing -> returnError ConversionFailed f ("Invalid Discord ID: " ++ T.unpack s)
