{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Activity
  ( updateReactionActivity
  , updateMessageActivity
  ) where

import Vesbot.Utils (showT, liftIO)
import Vesbot.Logging as Logging (echo)
import Vesbot.Activity.Database (DBConnection, incrementUserMessageActivity, incrementUserReactionActivity)

import Data.Time (UTCTime)

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Control.Monad (unless)

updateMessageActivity :: DBConnection -> Message -> IO ()
updateMessageActivity dbconn msg = do
  let mgid = messageGuildId msg
      uid = userId . messageAuthor $ msg
  case mgid of
    Nothing  -> return () -- if the message wasn't sent in a guild, we don't care
    Just gid -> incrementUserMessageActivity dbconn gid uid 1 -- TODO: score messages in some way?

updateReactionActivity :: DBConnection -> ReactionInfo -> DiscordHandler ()
updateReactionActivity dbconn (ReactionInfo rUid mGid cid oMid _) = do
  ruOrErr <- restCall $ R.GetUser rUid -- the user that reacted (or an error)
  oMsgOrErr <- restCall $ R.GetChannelMessage (cid, oMid) -- the original message that was reacted to (or an error)
  case ruOrErr of
    Left err -> Logging.echo $
      "Failed to get the reacting user when trying update a user's emoji reaction score. \
      \Discord returned the error:\n" <> showT err
    Right rUsr ->
      unless (userIsBot rUsr) $ case mGid of -- we don't care if a bot reacts to a message 
        Nothing -> return () -- if the reaction didn't happen in a guild, we don't care
        Just gid -> case oMsgOrErr of
          Left err ->
            Logging.echo $
              "Failed to get the original message when trying to update a user's emoji reaction score. \
              \Discord returned the error:\n" <> showT err
          Right oMsg -> do -- the message the user reacted to.
            let op = messageAuthor oMsg
            -- self reactions don't count and we don't care to track how many reactions bots get.
            unless (userId op == rUid || userIsBot op) $
              liftIO $ incrementUserReactionActivity dbconn gid (userId op) 1 -- TODO: different emojis have different scores?
