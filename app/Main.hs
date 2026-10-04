{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where
import Discord
import Discord.Types
import Discord.Interactions
import Data.List (find)
import Control.Monad (forM_)
import Utils ()
import qualified Discord.Requests as R
import SlashCommands
import Responses
import Utils
import qualified Database.SQLite.Simple as SQL
import Predictions (initDb)

main :: IO ()
main = do
  tok <- getToken -- see Utils.hs
  gId <- getGuildId  -- see Utils.hs
  keywordResponseData <- parseJSON _KEYWORD_RESPONSE_FILEPATH :: IO (Maybe [KeywordResponseData])
  let keywordResponses = case keywordResponseData of
        Nothing  -> []
        Just res -> map createKeywordResponse res

  dbConn <- SQL.open "appdata/database/data.db"
  initDb dbConn

  botTerminationError <- runDiscord $ def
    { discordToken = tok
    , discordOnEvent = onDiscordEvent dbConn keywordResponses gId
    , discordOnEnd = do
        echo "Bot has disconnected. Cleaning up..."
        SQL.close dbConn
    , discordGatewayIntent = def { gatewayIntentMessageContent = True }
    }

  echo $ "A fatal error occurred: " <> botTerminationError

-- EVENTS

onDiscordEvent :: SQL.Connection -> [KeywordResponse] -> GuildId -> Event -> DiscordHandler ()
onDiscordEvent conn resList gId = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId gId
  InteractionCreate intr                         -> onInteractionCreate conn intr
  MessageCreate     mess                         -> onMessageCreate resList mess
  _                                              -> return ()

-- Registers the application commands defined in Commands.hs when the bot is ready.
onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady appId gId = do
  echo "Bot ready!"
  
  -- mySlashCommands comes from SlashCommands.hs
  appCmdRegistrations <- mapM tryRegistering mySlashCommands

  case sequence appCmdRegistrations of
    Left err ->
      echo $ "Failed to register some commands" <> showT err

    Right cmds -> do
      echo $ "Registered " <> showT (length cmds) <> " command(s)."
      unregisterOutdatedCmds cmds

  where
  tryRegistering cmd = case commandRegistration cmd of
    Just reg -> restCall $ R.CreateGuildApplicationCommand appId gId reg
    Nothing  -> return . Left $ RestCallErrorCode 0 "" ""

  -- Unregisters commands that existed on the last iteration of the bot, but no longer exist.
  unregisterOutdatedCmds validCmds = do
    registered <- restCall $ R.GetGuildApplicationCommands appId gId
    case registered of
      Left err ->
        echo $ "Failed to get registered slash commands: " <> showT err

      Right cmds ->
        let validIds    = map applicationCommandId validCmds
            outdatedIds = filter (`notElem` validIds)
                        . map applicationCommandId
                        $ cmds
         in forM_ outdatedIds $
              restCall . R.DeleteGuildApplicationCommand appId gId

-- | Only supports application commands currently. When someone uses an application command, the function tries to look
-- it up in the list of the registered commands.
onInteractionCreate :: SQL.Connection -> Interaction -> DiscordHandler ()
onInteractionCreate conn = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {} } ->
      case
        find (\c -> applicationCommandDataName input == commandName c) mySlashCommands
      of
        Just found -> do
          commandHandler found conn cmd (optionsData input)

        Nothing ->
          echo "Somehow got unknown slash command (registrations out of date?)"
  _ ->
    return () -- Unexpected/unsupported interaction type

onMessageCreate :: [KeywordResponse] -> Message -> DiscordHandler ()
onMessageCreate resList mess = case (fromBot mess) of
  True -> return ()
  _    ->
    case
      find (\res -> mess `startsWith` (responseKeyword . responseData $ res)) resList
    of
      Just found ->
        responseHandler found mess
      _          -> return ()
