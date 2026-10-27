module Vesbot.Activity.Types
  ( UserActivity(..)
  ) where

import Vesbot.Database.Types()

import Discord.Types (UserId)

import Data.Int (Int64)
import qualified Database.SQLite.Simple as SQL


data UserActivity = UserActivity
  { activityUserId :: UserId
  , activityMessages :: Int64
  , activityReacts :: Int64
  , activityReactees :: Int64
  } deriving Show

-- data EmojiActivity = EmojiActivity
--  { emojiActivityEmojiId :: Either T.Text EmojiId
--  , emojiActivityTimesUsed :: Int64
--  }

-- See Vesbot.Database for the GuildId/UserId FromField instance.
instance SQL.FromRow UserActivity where
  fromRow = UserActivity <$> SQL.field <*> SQL.field <*> SQL.field <*> SQL.field

