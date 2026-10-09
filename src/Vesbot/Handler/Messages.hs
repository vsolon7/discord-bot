{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Messages where

import Vesbot.Config (Config, InitialEnv, envResponses, envDBConnection, cfgGuildId)
import Vesbot.Database (DBConnection)
import Vesbot.Utils (liftIO, fromBot)
import Vesbot.Logging as Logging (echo)
import Vesbot.Responses (responseData, responseKeyword, responseHandler)

import Discord
import Discord.Types

import qualified Data.Text as T
import Data.List (find)
import Data.Time (getCurrentTime)

messageHandler :: Config -> InitialEnv -> Message -> DiscordHandler ()
messageHandler cfg env =
  \message ->
    if fromBot message
      then return ()
    else do
      now <- liftIO getCurrentTime
      case find (\r -> message `startsWith` (responseKeyword . responseData $ r)) (envResponses env) of
        Nothing       -> return ()
        Just response -> responseHandler response message
      case messageGuildId message of
        Nothing ->
          Logging.echo
            "Received message does not have an associated Guild ID. The bot is only \
            \supposed to be used in a server!"
        Just gid ->
          if gid /= cfgGuildId cfg
            then Logging.echo "Received message with a different Guild ID than the Guild ID the bot was configured \
                              \ with. The bot is designed to be used on a single server!"
          else liftIO $ updateMessageActivity (envDBConnection env) message now
  where
    startsWith :: Message -> T.Text -> Bool
    startsWith mess t = t `T.isPrefixOf` (T.toLower . messageContent $ mess)
    
    -- TODO: This should probably be done in an 'activity tracker' module.
    updateMessageActivity :: DBConnection -> Message -> UTCTime -> IO ()
    updateMessageActivity = undefined

