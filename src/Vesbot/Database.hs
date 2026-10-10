module Vesbot.Database
  ( initDBConnection
  ) where

import Vesbot.Database.Types (DBConnection)

import qualified Vesbot.ArgumentTimer.Database as ArgumentTimer (initTable)
import qualified Vesbot.Notifications.Database as Notifications (initTable)

import Control.Concurrent.MVar (newMVar)
import Database.SQLite.Simple as SQL


initDBConnection :: FilePath -> IO DBConnection
initDBConnection databaseFile = do
  c <- SQL.open databaseFile
  threadSafeDBConnection <- newMVar c

  ArgumentTimer.initTable threadSafeDBConnection
  Notifications.initTable threadSafeDBConnection

  return threadSafeDBConnection
