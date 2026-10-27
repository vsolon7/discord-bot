module Vesbot.Activity
  ( modifyReactionActivity
  , updateMessageActivity
  , addEmojiUse
  ) where

import Vesbot.Activity.Database (DBConnection, incrementMessageActivity, modifyReactionActivity, addEmojiUse)

import Discord.Types

updateMessageActivity :: DBConnection -> Message -> IO ()
updateMessageActivity dbconn msg = do
  let mgid = messageGuildId msg
      uid = userId . messageAuthor $ msg
  case mgid of
    Nothing  -> return () -- if the message wasn't sent in a guild, we don't care
    Just _ -> incrementMessageActivity dbconn uid
