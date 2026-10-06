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
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Predictions
import Utils (showT, extractNumberOption, extractUserOption, echo, ephemeralResponseBasic)
import UnliftIO (liftIO)
import Control.Monad.Reader (ask, runReaderT)

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
  { viewCurrencyUser :: UserId }


initCurrencyTable :: SQL.Connection -> IO ()
initCurrencyTable conn =
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS currency(\
    \  user_id       TEXT PRIMARY KEY,\
    \  currency_amt  REAL NOT NULL) STRICT"


updateCurrency :: DbConnection -> CurrencyUpdate -> IO ()
updateCurrency dbconn cu = withDb dbconn $
  \conn -> do
    SQL.executeNamed conn
      "INSERT INTO currency (user_id, currency_amt) VALUES (:userid, :change)\
      \ON CONFLICT(user_id)\
      \DO UPDATE SET currency_amt = currency_amt + :change"
      [ ":change" SQL.:= (currencyUpdateAmount cu), ":userid" SQL.:= (showT . currencyUpdateUser $ cu) ]


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
      "Could not access the pay command's option fields."
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
      CurrencyQuery { currencyQueryAmount = userCurrency } <- getCurrency dbconn (payFromUser p)
      response <- do
        if userCurrency >= payAmount p then
          let response =
                "Paid " <> showT (payAmount p) <> " currency units to <@" <> showT (payToUser p) <> ">."
              remove = CurrencyUpdate (payFromUser p) ((-1) * (payAmount p))
              add = CurrencyUpdate (payToUser p) (payAmount p)
          in do
            void . forkIO $ do -- multithread the database updates so that we can send a response sooner
              updateCurrency dbconn remove
              updateCurrency dbconn add
            return response
        else do
          return "You don't have enough currency!"
      return (ephemeralResponseBasic response)
    Left err -> return (ephemeralResponseBasic err)


parseViewCurrencyCommand :: Interaction -> Either T.Text ViewCurrencyCommandData
parseViewCurrencyCommand (cmd@InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} }) = do
  dataValues <-
    maybeToEither
      "Could not access the view currency command's option fields."
      (optionsData input)
  user <-
    maybeToEither
      "Could not access user field in the view currency command."
      (extractUserOption "user" dataValues)
  return $
    ViewCurrencyCommandData user

parseViewCurrencyCommand _ = Left "Tried to parse a non slash command when parsing the view currency command."


createViewCurrencyResponse :: DbConnection -> Either T.Text ViewCurrencyCommandData -> IO InteractionResponse
createViewCurrencyResponse dbconn vccd =
  case vccd of
  Right v -> do
    c <- getCurrency dbconn (viewCurrencyUser v)
    let userPing = "<@" <> showT (viewCurrencyUser v) <> ">"
        response = userPing <> " has " <> showT (currencyQueryAmount c) <> " units of currency."
    return (ephemeralResponseBasic response)
  Left err -> return (ephemeralResponseBasic err)



addCurrencyFromReaction :: DbConnection -> ReactionInfo -> DiscordHandler ()
addCurrencyFromReaction dbconn reactInfo = do
  h <- ask -- get the DiscordHandle so we can fork a new process to do the slow restCall and database reads
  void . liftIO . forkIO $ do
    originalMessage <-
      runReaderT (restCall $ R.GetChannelMessage (reactionChannelId reactInfo, reactionMessageId reactInfo)) h
    reactingUser <-
      runReaderT (restCall $ R.GetUser (reactionUserId reactInfo)) h
    case reactingUser of
      Left err ->
        echo $
          "Failed to get the reacting user when trying to pay a user for an emoji reaction. \
          \Discord returned the error:\n" <> showT err
      Right usr ->
        if userIsBot usr
          then return ()
        else
          case originalMessage of
            Left err ->
              echo $
                "Failed to get the original message when trying to pay a user for an emoji reaction. \
                \Discord returned the error:\n" <> showT err
            Right message -> do
              let op = messageAuthor $ message
              -- you can't give yourself money by reacting to your message, and bots can't get money
              if (userId op == reactionUserId reactInfo || userIsBot op)
                then return ()
              else
                -- TODO: should different emojis give different amounts of currency?
                updateCurrency dbconn (CurrencyUpdate (userId op) 1)
