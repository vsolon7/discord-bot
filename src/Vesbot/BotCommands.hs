{-# LANGUAGE OverloadedStrings #-}
module Vesbot.BotCommands
  ( BotCommand(..)
  , slashCommands
  , messageCommands
  ) where


import Vesbot.Logging as Logging (LogType(..), log)
import Vesbot.Utils (showT, void, liftIO, ephemeralResponseBasic, forM_, threadDelay, announce)
import Vesbot.Parsing (extractIntegerOption, extractOptionsData, extractField)
import Vesbot.Config (Env(..), Config(..))
import Vesbot.BotCommands.Types
import Vesbot.ArgumentTimer.Commands (viewAC, resetAC)
import Vesbot.Predictions.Commands (makePrediction)
import Vesbot.Currency (currencyGuard, showC)
import Vesbot.Currency.Types (EmojiUpdate(..))
import Vesbot.Currency.Database (updateEmojis)
import Vesbot.Currency.Commands (viewCurrencyCommand, payCommand, viewRichestUsersCommand, viewEmojisCommand)
import Vesbot.Reminders.Commands (setReminder)
import Vesbot.Responses (RareEmoji(..), selectNRandomRareEmojis )

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions

import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Random.Stateful (uniformRM, globalStdGen)
import Control.Monad (when)


-- | Some are slash commands, some are message commands.
slashCommands :: Env -> Config -> [BotCommand]
slashCommands env cfg =
  [ ping
  , time
  , glorp
  , roll
  , gamble cfg env
  , viewAC (envDBConnection env) cfg
  , makePrediction env
  , setReminder env
  , viewCurrencyCommand (envDBConnection env) cfg
  , payCommand (envDBConnection env) cfg
  , viewRichestUsersCommand (envDBConnection env) cfg
  , viewEmojisCommand (envDBConnection env)
  ]

messageCommands :: Env -> [BotCommand]
messageCommands env =
  [ resetAC (envDBConnection env)
  ]


ping :: BotCommand
ping =
  basicSlashCommand
    "ping"
    "Responds 'pong' if the bot is alive"
    (return "pong!")


time :: BotCommand
time =
  basicSlashCommand
    "time"
    "Displays the Current time in UTC"
    (do x <- getCurrentTime; return $ "The current time is " <> showT x <> ".")


glorp :: BotCommand
glorp =
  basicSlashCommand
    "glorp"
    "Prints a giant glorp"
    (return $ ".\n" <> giantGlorp)
  where
    giantGlorp :: T.Text
    giantGlorp = "I⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⢠⣄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠘⢻⠄⠄⠄⠄⠄⠄⠄⢡⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠄⠘⡀⠄⠄⠄⠄⠄⠄⠘⡆⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠄⠄⣇⠄⠄⣀⣀⣀⣤⣄⣷⣤⡀⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⣀⣸⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣠⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⢀⣸⣿⣿⣿⣿⣿⢹⣿⣿⣿⣿⣿⣿⠹⣿⣿⣷⣤⡄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠰⣿⣍⡻⣿⠿⠿⠾⢾⣿⣿⣿⣿⣿⣿⠿⠛⠻⢿⡟⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⣿⣏⣰⣿⣦⣀⠄⠄⢹⣿⣿⣿⣿⣏⡀⠄⣀⣿⣿⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⣫⣭⡄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠸⠿⠶⠶⢿⣿⣿⣿⣿⣿⣅⣖⣼⣿⣿⣿⣯⣽⣭⠉⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⢸⣿⣿⣿⣿⣿⣿⣿⣿⣭⣤⣽⣿⣿⣿⣿⢿⣻⡇⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⣼⣿⣿⣿⣿⣿⣿⡿⢿⡿⣿⢿⣟⣛⣿⣷⣿⣿⣿⣂⠄⠄I\n"
              <> "I⠄⠄⠄⠄⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠄I\n"


roll :: BotCommand
roll = SlashCommand
  { commandName = cmdName
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
    let intOrErr = parseCommand intr opts
    case intOrErr of
      Left err -> do
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (ephemeralResponseBasic err) 
      Right n -> do
        mUsr <- liftIO $ tryToGetUser (interactionUser intr) -- this command is only going to work for guild members
        case mUsr of
          Nothing -> return ()
          Just _ -> do
            sendRollReply n intr
  }
  where
    cmdName = "roll"
    opt = OptionValueInteger "n" Nothing "The number of sides the die has" Nothing True (Left False) (Just 1) Nothing
    reg = -- Command registration
      CreateApplicationCommandChatInput
        cmdName
        Nothing
        ("Roll an n-sided die and print the result")
        Nothing 
        (Just $ OptionsValues [opt])
        Nothing
        (Just False)

    parseCommand :: Interaction -> Maybe OptionsData -> Either T.Text Integer
    parseCommand InteractionApplicationCommand {} opts = do
      optValues <- extractOptionsData cmdName opts
      extractField extractIntegerOption "n" cmdName optValues
    parseCommand _ _ = Left "Tried to parse a non slash command on an interaction that is not an application command."

    tryToGetUser :: MemberOrUser -> IO (Maybe User)
    tryToGetUser mou = case mou of
      MemberOrUser (Right _) -> do
        liftIO $ Logging.log Warn $
          "User that rolled a die was not a member of a guild. Note that the bot is only\
          \ designed to be used in a server."
        return Nothing
      MemberOrUser (Left gm) ->
        case memberUser gm of
          Nothing -> do
            Logging.log Error $
              "Could not obtain the user data of a user that rolled a die."
            return Nothing
          Just u -> return (Just u)

    sendRollReply :: Integer -> Interaction -> DiscordHandler ()
    sendRollReply n intr = do
      rand <- liftIO $ uniformRM (1, n) globalStdGen :: DiscordHandler Integer
      let response = "You rolled a " <> showT rand <> " on the " <> "d" <> showT n <> "."
      Logging.log Info $ "User rolled a " <> "d" <> showT n <> " with the roll command and rolled a " <> showT rand <> "."

      res <- restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic response)
      case res of
        Left err ->
          Logging.log Error $
            "The REST call to send a roll response failed. Discord returned the error:\n" <> showT err
        Right _ -> return ()

gamble :: Config -> Env -> BotCommand
gamble cfg env = SlashCommand
  { commandName = cmdName
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
    let intOrErr = parseCommand intr opts
    case intOrErr of
      Left err -> do
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (ephemeralResponseBasic err) 
      Right n -> do
        mUsr <- liftIO $ tryToGetUser (interactionUser intr) -- this command is only going to work for guild members
        case mUsr of
          Nothing -> return ()
          Just usr -> do
            let uid = userId usr
            currencyGuard (cfgCurrencyInfo cfg) cmdName (envDBConnection env) uid cmdCost intr $ do
            rollResult <- rollResponse n intr

            -- get a prize if you meet the win condition
            when (winCondition n rollResult) $
              onWin intr uid n
  }
  where
    cmdName = "gamble"
    cmdCost = 0.02
    minRoll = 50
    opt = OptionValueInteger "n" Nothing "The number of sides the die has (must be >= 50)" Nothing True (Left False) (Just minRoll) Nothing
    reg = -- Command registration
      CreateApplicationCommandChatInput
        cmdName
        Nothing
        ("Gamble by rolling an n-sided die. Cost: " <> showC (cfgCurrencyInfo cfg) cmdCost)
        Nothing 
        (Just $ OptionsValues [opt])
        Nothing
        (Just False)

    parseCommand :: Interaction -> Maybe OptionsData -> Either T.Text Integer
    parseCommand InteractionApplicationCommand {} opts = do
      optValues <- extractOptionsData cmdName opts
      extractField extractIntegerOption "n" cmdName optValues
    parseCommand _ _ = Left "Tried to parse a non slash command on an interaction that is not an application command."

    tryToGetUser :: MemberOrUser -> IO (Maybe User)
    tryToGetUser mou = case mou of
      MemberOrUser (Right _) -> do
        liftIO $ Logging.log Warn $
          "User that rolled a die was not a member of a guild. Note that the bot is only\
          \ designed to be used in a server."
        return Nothing
      MemberOrUser (Left gm) ->
        case memberUser gm of
          Nothing -> do
            Logging.log Error $
              "Could not obtain the user data of a user that rolled a die."
            return Nothing
          Just u -> return (Just u)

    rollResponse :: Integer -> Interaction -> DiscordHandler Integer
    rollResponse n intr = do
      rand <- liftIO $ uniformRM (1, n) globalStdGen :: DiscordHandler Integer
      let response = "You rolled a " <> showT rand <> " on the " <> "d" <> showT n <> "."
      Logging.log Info $ "User rolled a " <> "d" <> showT n <> " with the roll command and rolled a " <> showT rand <> "."

      res <- restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic response)
      case res of
        Left err ->
          Logging.log Error $
            "The REST call to send a roll response failed. Discord returned the error:\n" <> showT err
        Right _ -> return ()

      return rand

    winCondition n dieRoll = n >= minRoll && dieRoll == n
    computePayout n = fromInteger (n `div` 20)

    onWin intr uid n = do
      let iTok = interactionToken intr
      maybeMsg <- restCall $ -- get the message the bot responded with.
        R.GetOriginalInteractionResponse
        (interactionApplicationId intr)
        iTok
      case maybeMsg of
        Left err ->
          Logging.log Error $
            "A REST call failed when trying to get the bot's reply message to the `" <> showT cmdName <> "` command.\
            \Discord sent the following error message:\n" <> showT err
        Right msg -> do
          -- react with some rare emojis
          reacts <- liftIO $ selectNRandomRareEmojis (computePayout n) (cfgRareEmoji cfg)
          forM_ reacts $ \r -> do
            rareEmojiHandler r msg
            threadDelay 250000 -- wait 0.25 seconds in between reacts

          let userPing = "<@" <> showT uid <> ">"
              eUpdate = EmojiUpdate uid [(rareEmojiId re, 1) | re <- reacts ]
              cid = showT . messageChannelId $ msg
              mid = showT . messageId $ msg
              link = "https://discord.com/channels/" <> (showT . cfgGuildId $ cfg) <> "/" <> cid <> "/" <> mid
              message = "HOLY. " <> userPing <> " just rolled a " <> "d" <> showT n <> " and rolled the higest value. CHECK IT: " <> link
          liftIO $ updateEmojis (envDBConnection env) eUpdate
          announce (cfgAnnouncementCID cfg) message

