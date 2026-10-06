{-# LANGUAGE OverloadedStrings #-}
module Wagers where

import Discord
import Discord.Types
import Discord.Handle
import Discord.Handle
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Interactions
import qualified Discord.Requests as R
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.Internal as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import Data.Time.Clock (getCurrentTime, addUTCTime, diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime
import Data.Ratio
import Predictions (getUserData, parseDateInput, maybeToEither)
import Utils (extractStringOption, extractIntegerOption, extractNumberOption, showT, ephemeralResponseBasic)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)

data WagerCommandData = WagerCommandData
  { wagerContent :: T.Text
  , wagerBet :: Double
  , wagerOdds :: Maybe Rational
  , wagerDueDate :: UTCTime
  , wagerOfferingUserId :: UserId
  , wagerGuild :: GuildId
  , wagerChannel :: ChannelId
  } deriving Show

data AcceptedWager = AcceptedWager
  { wagerCommandData :: WagerCommandData
  , wagerAcceptingUser :: UserId
  , wagerAcceptedDate :: UTCTime
  , wagerOfferMessageId :: MessageId
  } deriving Show

instance SQL.ToRow AcceptedWager where
  toRow (AcceptedWager initialOffer taker acceptDate wmid) =
    [ SQL.SQLText (wagerContent initialOffer)
    , SQL.SQLFloat (wagerBet initialOffer)
    , fromMaybe SQL.SQLNull (fmap (SQL.SQLFloat . fromRational) (wagerOdds initialOffer))
    , SQL.SQLText (showT . formatUTC . wagerDueDate $ initialOffer)
    , SQL.SQLText (showT . wagerOfferingUserId $ initialOffer)
    , SQL.SQLText (showT . wagerGuild $ initialOffer)
    , SQL.SQLText (showT . wagerChannel $ initialOffer)
    , SQL.SQLText (showT taker)
    , SQL.SQLText (showT acceptDate)
    , SQL.SQLText (showT wmid)
    ]
      where
        formatUTC t = fromInteger . ceiling . utcTimeToPOSIXSeconds $ t


initWagerTable :: SQL.Connection -> IO ()
initWagerTable conn =
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS wagers(\
    \  id             INTEGER PRIMARY KEY,\
    \  content        TEXT NOT NULL,\
    \  bet_amount     REAL NOT NULL,\
    \  wager_odds     REAL,\
    \  due_at         INTEGER NOT NULL,\
    \  offering_user  TEXT NOT NULL,\
    \  guild_id       TEXT NOT NULL,\
    \  channel_id     TEXT NOT NULL,\
    \  accepting_user TEXT NOT NULL,\
    \  accepted_at    INTEGER NOT NULL,\
    \  reply_mess_id  TEXT NOT NULL,\
    \  notified       INTEGER NOT NULL DEFAULT 0) STRICT"


parseOddsInput :: String -> Maybe Rational
parseOddsInput inpt =
  let
    (n1,r) = span isDigit inpt
  in case r of
    ':':n2 -> Just (read n1 % read n2)
    _      -> Nothing


parseWagerCommand :: UTCTime -> Interaction -> Either T.Text WagerCommandData
parseWagerCommand currTime (cmd@InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} }) = do
  dataValues <-
    maybeToEither
      "Could not access the wager command's option fields."
      (optionsData input)
  wagerText <-
    maybeToEither
      "Could not access claim field."
      (extractStringOption "claim" dataValues)
  betAmount <-
    maybeToEither
      "Could not access bet amount field."
      (extractNumberOption "amount" dataValues)
  let oddsText = extractStringOption "odds" dataValues
  dueDateText <-
    maybeToEither
      "Could not access date field."
      (extractStringOption "date" dataValues)
  dueDateUTC <-
    maybeToEither
      "Error parsing prediction date. Here are the formatting options.\n\
      \**Relative time:** \"in #w\" or \"in #d\" or \"in #h\" or \"in #m\", where # is a positive integer.\
      \ \"w\" stands for weeks, \"d\" stands for days, \"h\" stands for hours, and \"m\" stands for minutes.\n\
      \**Absolute time:** \"on month-day-year HH:MM <timezone>\", where the timezone is optional.\
      \If no timezone is entered, the bot will default to UTC. Timezone examples are CST, CDT, EST, etc."
      (parseDateInput currTime . T.unpack $ dueDateText)
  uid <-
    maybeToEither
      "Could not get user data. Note that this command can only be used in a server."
      (getUserData . interactionUser $ cmd)
  gid <-
    maybeToEither
      "Error accessing Guild ID. Note that this command can only be used in a server."
      (interactionGuildId cmd)
  cid <-
    maybeToEither
      "Error accessing Channel ID."
      (interactionChannelId cmd)
  return $
    WagerCommandData
      { wagerContent = wagerText
      , wagerBet = betAmount
      , wagerOdds = case oddsText of
          Just cs -> parseOddsInput (T.unpack cs)
          _       -> Nothing
      , wagerDueDate = dueDateUTC
      , wagerOfferingUserId = uid
      , wagerGuild = gid
      , wagerChannel = cid
      }

parseWagerCommand _ _ = Left "Tried to parse an interaction that is not a chat input slash command when parsing the wager command."


-- | Generate the bot reply. parsePredictionCommand returns either an error message or a record of the
-- relevant prediction command data. We must either respond with an (ephemeral) error, or else form the bot reply
-- in the case that the user actually predicted a future event.
createInitialWagerResponse :: UTCTime -> Either T.Text WagerCommandData -> InteractionResponse
createInitialWagerResponse currTime wcd =
  case wcd of
    Right w ->
      if currTime < wagerDueDate w then
        let
          userPing = "<@" <> showT (wagerOfferingUserId w) <> ">"
          dueDateUTC = showT . ceiling . utcTimeToPOSIXSeconds $ wagerDueDate w
          dueDateTimeStampRel = "<t:" <> dueDateUTC <> ":R>"
          dueDateTimeStampAbs = "<t:" <> dueDateUTC <> ":f>"
          oddsText = case wagerOdds w of
            Nothing -> "1 : 1"
            Just r  -> (showT . numerator $ r) <> " : " <> (showT . denominator $ r)
          response =
            userPing <> " has offered a wager to anyone who wants to take it!\n" <>
            "They are betting " <> showT (wagerBet w) <> " units of currency at " <> oddsText <> " odds that \
            \on or before " <> dueDateTimeStampAbs <> ", which is " <> dueDateTimeStampRel <>
            ", the following will occur:\n" <> wagerContent w <> "."
        in
          InteractionResponseChannelMessage $
            InteractionResponseMessage
              Nothing
              (Just response)
              Nothing
              Nothing
              Nothing
              (Just [ActionRowButtons [takeButton]])
              Nothing
      else
        ephemeralResponseBasic "You can only predict events in the future!"
    Left err -> ephemeralResponseBasic err
    where
      takeButton =
        Button
          "b"
          False
          ButtonStylePrimary
          (Just "Accept Wager")
          Nothing
