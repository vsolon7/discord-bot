{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Currency.Database where

import Vesbot.Utils (showT)
import Vesbot.Database.Types (DBConnection, withDB)
import Vesbot.Currency.Types (CurrencyQuery(..), CurrencyUpdate(..))

import Discord.Types

import Database.SQLite.Simple as SQL

initCurrencyTable :: DBConnection -> IO ()
initCurrencyTable dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS currency(\
    \  user_id       TEXT NOT NULL PRIMARY KEY,\
    \  currency_amt  REAL NOT NULL DEFAULT 0) STRICT"

getCurrency :: DBConnection -> UserId -> IO CurrencyQuery
getCurrency dbconn uid = withDB dbconn $
  \conn -> do
    res <- SQL.query conn
      "SELECT currency_amt FROM currency WHERE user_id = ?"
      [showT uid] :: IO [SQL.Only (Maybe Double)]
    case res of
      [SQL.Only (Just amt)] -> return (CurrencyQuery uid amt)
      _                     -> return (CurrencyQuery uid 0)

updateCurrency :: DBConnection -> CurrencyUpdate -> IO ()
updateCurrency dbconn cu = withDB dbconn $
  \conn -> do
    SQL.executeNamed conn
      "INSERT INTO currency (user_id, currency_amt) VALUES (:userid, :change)\
      \ON CONFLICT(user_id)\
      \DO UPDATE SET currency_amt = currency_amt + :change"
      [ ":change" SQL.:= (currencyUpdateAmount cu), ":userid" SQL.:= (showT . currencyUpdateUser $ cu) ]

getRichestUsers :: DBConnection -> IO [CurrencyQuery]
getRichestUsers dbconn = withDB dbconn $
  \conn -> SQL.query_ conn
    "SELECT user_id, currency_amt FROM currency \
    \ORDER BY currency_amt DESC \
    \LIMIT 10"
