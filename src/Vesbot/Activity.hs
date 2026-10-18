{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Activity
  ( modifyReactionActivity
  , updateMessageActivity
  , addEmojiUse
  ) where

import Vesbot.Utils (showT, liftIO)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Activity.Database (DBConnection, incrementMessageActivity, modifyReactionActivity, addEmojiUse)

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
    Just gid -> incrementMessageActivity dbconn uid
