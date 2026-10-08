{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Reactions where

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Time
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Monad.Reader (ask, runReaderT)

import Vesbot.Database
import Vesbot.Utils (echo, showT)
import Vesbot.Currency.Types
import Vesbot.Currency.Database (updateCurrency, updateReactionActivity)


reactionHandler :: DbConnection -> GuildId -> ReactionInfo -> DiscordHandler ()
reactionHandler dbconn gid reactInfo = do
  now <- liftIO getCurrentTime
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
              else do
                -- TODO: should different emojis give different amounts of currency?
                  updateReactionActivity dbconn gid (userId op) now
                  updateCurrency dbconn (CurrencyUpdate (userId op) 1)
