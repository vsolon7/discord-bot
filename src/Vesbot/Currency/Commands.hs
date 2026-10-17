{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Currency.Commands
  ( payCommand
  , viewCurrencyCommand
  , viewRichestUsersCommand
  ) where

import Vesbot.Utils (showT, maybeToEither, ephemeralResponseBasic, liftIO, void, forM)
import Vesbot.Parsing (getUserData, extractUserOption, extractNumberOption)
import Vesbot.Config (Config(..), CurrencyConfig(..))
import Vesbot.Database.Types (DBConnection)
import Vesbot.Currency.Types (ViewCurrencyCommandData(..), PayCommandData(..), CurrencyQuery(..), CurrencyUpdate(..))
import Vesbot.Currency.Database (getCurrency, updateCurrency, getRichestUsers)
import Vesbot.BotCommands.Types (BotCommand(..))

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions

import qualified Data.Text as T
import Data.List (intersperse)


parseViewCurrencyCommand :: Interaction -> Maybe OptionsData -> Either T.Text ViewCurrencyCommandData
parseViewCurrencyCommand cmd@(InteractionApplicationCommand {}) opts = do
  dataValues <-
    maybeToEither
      "Could not access the view currency command's option fields."
      opts
  user <-
    maybeToEither
      "Could not access user field in the view currency command."
      (extractUserOption "user" dataValues)
  return $
    ViewCurrencyCommandData user


parsePayCommand :: Interaction -> Maybe OptionsData -> Either T.Text PayCommandData
parsePayCommand cmd@(InteractionApplicationCommand {}) opts = do
  dataValues <-
    maybeToEither
      "Could not access the pay command's option fields."
      opts
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


createPaymentResponse :: DBConnection -> CurrencyConfig -> Either T.Text PayCommandData -> IO InteractionResponse
createPaymentResponse dbconn ccfg pcd =
  let cs = currencySymbol ccfg
  in  case pcd of
        Left err -> return (ephemeralResponseBasic err)
        Right p -> do
          CurrencyQuery { currencyQueryAmount = userCurrency } <- getCurrency dbconn (payFromUser p)
          response <- do
            if userCurrency >= payAmount p then
              let response =
                    "Paid " <> cs <> showT (payAmount p) <> " to <@" <> showT (payToUser p) <> ">."
                  remove = CurrencyUpdate (payFromUser p) ((-1) * (payAmount p))
                  add = CurrencyUpdate (payToUser p) (payAmount p)
              in do
                  updateCurrency dbconn remove -- TODO: can be slightly more efficient as a single SQL transaction...
                  updateCurrency dbconn add
                  return response
              else do
                return $ "You don't have enough " <> cs <> "!"
          return (ephemeralResponseBasic response)


createViewCurrencyResponse :: DBConnection -> CurrencyConfig -> Either T.Text ViewCurrencyCommandData -> IO InteractionResponse
createViewCurrencyResponse dbconn ccfg vccd =
  let cs = currencySymbol ccfg
  in  case vccd of
        Left err -> return (ephemeralResponseBasic err)
        Right v -> do
          c <- getCurrency dbconn (viewCurrencyUser v)
          let userPing = "<@" <> showT (viewCurrencyUser v) <> ">"
              response = userPing <> " has " <> cs <> showT (currencyQueryAmount c) <> "."
          return (ephemeralResponseBasic response)


createViewRichestUsersResponse :: DBConnection -> CurrencyConfig -> IO InteractionResponse
createViewRichestUsersResponse dbconn ccfg =
  let cs = currencySymbol ccfg
  in do
    qs <- getRichestUsers dbconn
    let rs =
          flip map (zip [1..] qs) $ \(n, q) ->
            let userPing = "<@" <> showT (currencyQueryUser q) <> ">"
                r = showT n <> ". " <> userPing <> ": " <> cs <> showT (currencyQueryAmount q) <> "."
            in r
        response = T.concat (intersperse "\n" rs :: [T.Text])
        pingPerms = AllowedMentions False False False [] [] True
        message = InteractionResponseMessage (Just False) (Just response) Nothing (Just pingPerms) Nothing Nothing Nothing []
    return (InteractionResponseChannelMessage message)
  


payCommand :: DBConnection -> Config -> BotCommand
payCommand dbconn cfg = SlashCommand
  { commandName = "pay"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
      let pcd = parsePayCommand intr opts :: Either T.Text PayCommandData
      botReply <- liftIO $ createPaymentResponse dbconn (cfgCurrencyInfo cfg) pcd
      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply

      -- TODO: Database stuff
  }
    where
      cs = currencySymbol . cfgCurrencyInfo $ cfg
      o1 = OptionValueUser "pay_to" Nothing ("Who to send the " <> cs <> " to") Nothing True
      o2 = OptionValueNumber "amount" Nothing "Amount to send" Nothing True (Left False) (Just 0) Nothing
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "pay"
          Nothing
          ("Send " <> cs <> " to a user")
          Nothing 
          (Just $ OptionsValues [o1, o2])
          Nothing
          (Just False)


viewCurrencyCommand :: DBConnection -> Config -> BotCommand
viewCurrencyCommand dbconn cfg = SlashCommand
  { commandName = "viewcurrency"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
      let vccd = parseViewCurrencyCommand intr opts :: Either T.Text ViewCurrencyCommandData
      botReply <- liftIO $ createViewCurrencyResponse dbconn (cfgCurrencyInfo cfg) vccd
      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply
  }
    where
      cs = currencySymbol . cfgCurrencyInfo $ cfg
      o1 = OptionValueUser "user" Nothing "User to check" Nothing True
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "viewcurrency"
          Nothing
          ("Check how much " <> cs <> " someone has")
          Nothing
          (Just $ OptionsValues [ o1 ])
          Nothing
          (Just False)


viewRichestUsersCommand :: DBConnection -> Config -> BotCommand
viewRichestUsersCommand dbconn cfg =
  let cs = currencySymbol . cfgCurrencyInfo $ cfg
  in  SlashCommand
        { commandName = "rich"
        , commandRegistration = createChatInput "rich" ("View the users with the most " <> cs)
        , slashCommandHandler = \intr _ -> do
            botReply <- liftIO $ createViewRichestUsersResponse dbconn (cfgCurrencyInfo cfg)
            void . restCall $
              R.CreateInteractionResponse
                (interactionId intr)
                (interactionToken intr)
                botReply
        }
