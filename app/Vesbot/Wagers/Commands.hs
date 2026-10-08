{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Wagers.Commands where

import Discord
import Discord.Types
import Discord.Internal.Types.Interactions
import Discord.Internal.Types.ApplicationCommands
import qualified Discord.Requests as R
import Data.Time
import Data.Ratio
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.Char (isDigit)
import qualified Data.Text as T

import Vesbot.Utils
import Vesbot.Parsing.Time
import Vesbot.Wagers.Types
import Vesbot.Wagers.Database
import Vesbot.SlashCommands.Types


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
          dueDateUTC = showT . formatUTCTime $ wagerDueDate w
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


makeWager :: SlashCommand
makeWager = SlashCommand
  { commandName = "wager"
  , commandRegistration = Just reg
  , commandHandler = \dbconn wake intr _ -> do
      currTime <- liftIO getCurrentTime
      let iTok = interactionToken intr -- save the interaction token to get the message ID of the reply later
      let pcd = parseWagerCommand currTime intr :: Either T.Text WagerCommandData
      let botReply = createInitialWagerResponse currTime pcd

      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply

      -- TODO: Database stuff
  }
    where
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "wager"
          Nothing
          "Offer a wager!"
          Nothing (
            Just (
              OptionsValues
                [
                  OptionValueString
                    "claim"
                    Nothing
                    "What you are betting will happen"
                    Nothing
                    True
                    (Left False)
                    (Just 1)
                    Nothing
                , OptionValueString
                    "date"
                    Nothing
                    "\"in #[w|d|h|m]\" or \"on MM-DD-YYYY HH:MM <timezone>\""
                    Nothing
                    True
                    (Left False)
                    (Just 1)
                    Nothing
                , OptionValueNumber
                    "amount"
                    Nothing
                    "Amount of currency you're betting"
                    Nothing
                    True
                    (Left False)
                    (Just 0)
                    Nothing
                , OptionValueString
                    "odds"
                    Nothing
                    "(Optional) odds you're giving"
                    Nothing
                    False
                    (Left False)
                    (Just 3)
                    Nothing
                ]
              )
            )
          Nothing
          (Just False)
