{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Currency.Commands where

import Discord
import Discord.Types
import Discord.Internal.Types.Interactions
import Discord.Internal.Types.ApplicationCommands
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import qualified Discord.Requests as R
import qualified Data.Text as T

import Vesbot.Utils
import Vesbot.Database
import Vesbot.Parsing.Time
import Vesbot.Currency.Types
import Vesbot.Currency.Database
import Vesbot.SlashCommands.Types


createPaymentResponse :: DbConnection -> Either T.Text PayCommandData -> IO InteractionResponse
createPaymentResponse dbconn pcd =
  case pcd of
    Right p -> do
      CurrencyQuery { currencyQueryAmount = userCurrency } <- getCurrency dbconn (payFromUser p)
      response <- do
        if userCurrency >= payAmount p then
          let response =
                "Paid λ" <> showT (payAmount p) <> " to <@" <> showT (payToUser p) <> ">."
              remove = CurrencyUpdate (payFromUser p) ((-1) * (payAmount p))
              add = CurrencyUpdate (payToUser p) (payAmount p)
          in do
            void . forkIO $ do -- multithread the database updates so that we can send a response sooner
              updateCurrency dbconn remove
              updateCurrency dbconn add
            return response
        else do
          return "You don't have enough $λ!"
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
        response = userPing <> " has λ" <> showT (currencyQueryAmount c) <> "."
    return (ephemeralResponseBasic response)
  Left err -> return (ephemeralResponseBasic err)


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


createCurrencyDropMessage :: ChannelId -> CurrencyDropResults -> R.ChannelRequest Message
createCurrencyDropMessage cid (CurrencyDropResults winnerChange otherChanges) =
  let winnerStatement =
        case winnerChange of
          Nothing -> "There was no winner!"
          Just wc -> "Winner: <@" <> (showT . currencyUpdateUser $ wc) <> ">: +" <> (showT . currencyUpdateAmount $ wc)
      otherUpdate c = "<@" <> (showT . currencyUpdateUser $ c) <> ">: +" <> (showT . currencyUpdateAmount $ c)
      otherUpdateStatement = T.concat [otherUpdate c <> "\n" | c <- otherChanges]
      content =
        "A currency drop happened!\n" <> winnerStatement <> "\n" <>
        "Users with equity in the drop:" <> otherUpdateStatement
  in
    R.CreateMessage
      cid
      content


payCommand :: SlashCommand
payCommand = SlashCommand
  { commandName = "pay"
  , commandRegistration = Just reg
  , commandHandler = \dbconn wake intr _ -> do
      let pcd = parsePayCommand intr :: Either T.Text PayCommandData
      botReply <- liftIO (createPaymentResponse dbconn pcd)
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
          "pay"
          Nothing
          "Send currency to a user"
          Nothing (
            Just (
              OptionsValues
                [
                  OptionValueUser
                    "pay_to"
                    Nothing
                    "Who to send the currency to"
                    Nothing
                    True
                , OptionValueNumber
                    "amount"
                    Nothing
                    "Amount to send"
                    Nothing
                    True
                    (Left False)
                    (Just 0)
                    Nothing
                ]
              )
            )
          Nothing
          (Just False)


viewCurrency :: SlashCommand
viewCurrency = SlashCommand
  { commandName = "viewcurrency"
  , commandRegistration = Just reg
  , commandHandler = \dbconn wake intr _ -> do
      let vccd = parseViewCurrencyCommand intr :: Either T.Text ViewCurrencyCommandData
      botReply <- liftIO (createViewCurrencyResponse dbconn vccd)
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
          "viewcurrency"
          Nothing
          "Check how much money someone has"
          Nothing (
            Just (
              OptionsValues
                [
                  OptionValueUser
                    "user"
                    Nothing
                    "User to check"
                    Nothing
                    True
                ]
              )
            )
          Nothing
          (Just False)
