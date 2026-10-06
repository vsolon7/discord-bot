{-# LANGUAGE OverloadedStrings #-}
module Currency where

import Discord
import Discord.Types
import Discord.Handle
import Discord.Handle
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Interactions
import qualified Discord.Requests as R
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.Internal as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Data.Text as T
import Predictions
import Utils (showT, extractNumberOption, extractUserOption)

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


initCurrencyTable :: SQL.Connection -> IO ()
initCurrencyTable conn =
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS currency(\
    \  id            INTEGER PRIMARY KEY,\
    \  user_id       TEXT NOT NULL,\
    \  currency_amt  REAL NOT NULL) STRICT"


updateCurrency :: DbConnection -> CurrencyUpdate -> IO ()
updateCurrency dbconn cu = withDb dbconn $
  \conn -> do
    SQL.executeNamed conn
      "UPDATE currency SET currency_amt = currency_amt + :increase \
      \WHERE user_id = :userid"
      [ ":increase" SQL.:= (currencyUpdateAmount cu), ":userid" SQL.:= (showT . currencyUpdateUser $ cu) ]


getCurrency :: DbConnection -> UserId -> IO CurrencyQuery
getCurrency dbconn uid = withDb dbconn $
  \conn -> do
    res <-
      SQL.queryNamed conn
        "SELECT currency_amt FROM currency WHERE user_id = :userid"
        [ ":userid" SQL.:= (showT uid) ] :: IO [SQL.Only (Maybe Double)]
    case res of
      [SQL.Only (Just amt)] -> return (CurrencyQuery uid amt)
      _                     -> return (CurrencyQuery uid 0)


parsePayCommand :: Interaction -> Either T.Text PayCommandData
parsePayCommand (cmd@InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} }) = do
  dataValues <-
    maybeToEither
      "Could not access the command's option fields."
      (optionsData input)
  payTo <-
    maybeToEither
      "Could not access \"pay to\" field."
      (extractUserOption "pay_to" dataValues)
  payAmount <-
    maybeToEither
      "Could not access payment amount field."
      (extractNumberOption "amount" dataValues)
  uid <-
    maybeToEither
      "Could not get user data. Note that this command can only be used in a server."
      (getUserData . interactionUser $ cmd)
  return $
    PayCommandData
      { payFromUser = uid
      , payToUser = payTo
      , payAmount = payAmount }

parsePayCommand _ = Left "Tried to parse a non slash command when parsing the pay command."


createPaymentResponse :: DbConnection -> Either T.Text PayCommandData -> IO InteractionResponse
createPaymentResponse dbconn pcd =
  case pcd of
    Right p -> do
      CurrencyQuery { currencyQueryAmount = userCurrency} <- getCurrency dbconn (payFromUser p)
      let response =
            if userCurrency >= payAmount p then
              "Paid " <> showT (payAmount p) <> " currency units to <@" <> showT (payToUser p) <> ">."
            else
              "You don't have enough currency!"
      return (ephemeralResponse response)
    Left err -> return (ephemeralResponse err)
    where
      ephemeralResponse t =
        InteractionResponseChannelMessage
          (interactionResponseMessageBasic t)
            { interactionResponseMessageFlags =
                Just (InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral])
            }
