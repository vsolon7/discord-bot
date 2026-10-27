module Vesbot.Currency.Types where

import Vesbot.Utils (showT)
import Vesbot.Database.Types()

import Discord.Types

import Database.SQLite.Simple as SQL
import qualified Data.Text as T
import Data.Int (Int64)

data CurrencyUpdate = CurrencyUpdate
  { currencyUpdateUser :: UserId
  , currencyUpdateAmount :: Double
  } deriving Show

data CurrencyQuery = CurrencyQuery
  { currencyQueryUser :: UserId
  , currencyQueryAmount :: Double
  } deriving Show

data EmojiUpdate = EmojiUpdate
  { emojiUpdateUser :: UserId
  , emojiUpdateAmounts :: [(T.Text, Int64)]
  }

data EmojiQuery = EmojiQuery
  { emojiQueryUser :: UserId
  , emojiQueryAmounts :: [(T.Text, Int64)]
  }

data PayCommandData = PayCommandData
  { payFromUser :: UserId
  , payToUser :: UserId
  , payAmount :: Double
  } deriving Show

newtype ViewCurrencyCommandData = ViewCurrencyCommandData
  { viewCurrencyUser :: UserId
  } deriving Show

instance SQL.FromRow CurrencyQuery where
  fromRow = CurrencyQuery <$> SQL.field <*> SQL.field

instance SQL.ToRow CurrencyUpdate where
  toRow (CurrencyUpdate uid amt) = [SQL.SQLText (showT uid), SQL.SQLFloat amt]


