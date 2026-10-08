module Vesbot.Currency.Types where
import Discord.Types

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

data RecentUserActivity = RecentUserActivity
  { recentActivityUserId :: UserId
  , recentActivityMessages :: Integer
  , recentActivityReacts :: Integer
  } deriving Show

data CurrencyDropResults = CurrencyDropResults
  { currencyDropWinner :: Maybe CurrencyUpdate
  , currencyDropEquity :: [CurrencyUpdate]
  } deriving Show
