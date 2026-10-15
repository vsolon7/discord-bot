{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Messages where

import Vesbot.Utils (liftIO, fromBot)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config (Config(..), Env(..))
import Vesbot.Database.Types (DBConnection)
import Vesbot.Activity (updateMessageActivity)
import Vesbot.Responses (responseData, responseKeyword, responseHandler)

import Discord
import Discord.Types

import qualified Data.Text as T
import Data.List (find)
import Data.Time (getCurrentTime)


messageHandler :: Config -> Env -> Message -> DiscordHandler ()
messageHandler cfg env =
  \msg ->
    if fromBot msg
      then return ()
    else do
      now <- liftIO getCurrentTime
      case find (\r -> msg `startsWith` (responseKeyword . responseData $ r)) (cfgResponses cfg) of
        Nothing       -> return ()
        Just response -> responseHandler response msg
      case messageGuildId msg of
        Nothing ->
          Logging.log Warn $ "Received message does not have an associated Guild ID! Ignoring it... the bot should \
          \only be used in a server."
        Just gid ->
          if gid /= cfgGuildId cfg
            then Logging.log Error $ "Received message with a different Guild ID than the Guild ID the bot was configured \
                              \ with. Does the token correspond to the Guild ID in the guildid file?"
          else liftIO $ updateMessageActivity (envDBConnection env) msg
  where
    startsWith :: Message -> T.Text -> Bool
    startsWith msg t = t `T.isPrefixOf` (T.toLower . messageContent $ msg)
