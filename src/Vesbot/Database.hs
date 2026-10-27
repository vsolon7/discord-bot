module Vesbot.Database
  ( initDBConnection
  , DBConnection
  , withDB
  , closeDBConnection
  ) where

import Vesbot.Database.Types (DBConnection, withDB)

import qualified Vesbot.ArgumentTimer.Database as ArgumentTimer (initTable)
import qualified Vesbot.Predictions.Database as Predictions (initTable)
import qualified Vesbot.Activity.Database as Activity (initUserActivityTable, initEmojiUseTable)
import qualified Vesbot.Currency.Database as Currency (initCurrencyTable, initEmojiTable)

import Control.Concurrent.MVar (newMVar)
import Database.SQLite.Simple as SQL


initDBConnection :: FilePath -> IO DBConnection
initDBConnection databaseFile = do
  c <- SQL.open databaseFile
  threadSafeDBConnection <- newMVar c

  ArgumentTimer.initTable threadSafeDBConnection
  Predictions.initTable threadSafeDBConnection
  Activity.initUserActivityTable threadSafeDBConnection
  Activity.initEmojiUseTable threadSafeDBConnection
  Currency.initCurrencyTable threadSafeDBConnection
  Currency.initEmojiTable threadSafeDBConnection

  return threadSafeDBConnection

closeDBConnection :: DBConnection -> IO ()
closeDBConnection dbconn = withDB dbconn SQL.close
