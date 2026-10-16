module Vesbot.Currency.Types where

import Vesbot.Utils (showT)
import Vesbot.Database.Types()

import Discord.Types
import Database.SQLite.Simple as SQL

data CurrencyUpdate = CurrencyUpdate
  { currencyUpdateUser :: UserId
  , currencyUpdateAmount :: Double
  } deriving Show

data CurrencyQuery = CurrencyQuery
  { currencyQueryUser :: UserId
  , currencyQueryAmount :: Double
  } deriving Show

data PayCommandData = PayCommandData
  { payFromUser :: UserId
  , payToUser :: UserId
  , payAmount :: Double
  } deriving Show

data ViewCurrencyCommandData = ViewCurrencyCommandData
  { viewCurrencyUser :: UserId
  } deriving Show

instance SQL.FromRow CurrencyQuery where
  fromRow = CurrencyQuery <$> SQL.field <*> SQL.field

instance SQL.ToRow CurrencyUpdate where
  toRow (CurrencyUpdate uid amt) = [SQL.SQLText (showT uid), SQL.SQLFloat amt]


