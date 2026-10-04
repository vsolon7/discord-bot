{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Predictions where

import Discord.Types
import Discord.Internal.Types.Interactions
import Text.Read (readMaybe)
import Utils (extractStringOption, showT)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL

type Username = T.Text

data PredictionData = PredictionData
  { predictionContent :: T.Text
  , predictionMadeDate :: UTCTime
  , predictionDueDate :: UTCTime
  , predictionUserId :: UserId
  , predictionUserName :: T.Text
  , predictionGuild :: GuildId
  , predictionChannel :: ChannelId
  } deriving Show

instance SQL.ToRow PredictionData where
  toRow p = [ SQL.SQLText (predictionContent p)
            , SQL.SQLText (showT . predictionMadeDate $ p)
            , SQL.SQLText (showT . predictionDueDate $ p)
            , SQL.SQLText (showT . predictionUserId $ p)
            , SQL.SQLText (predictionUserName p)
            , SQL.SQLText (showT . predictionGuild $ p)
            , SQL.SQLText (showT . predictionChannel $ p)
            ]

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

initDb :: SQL.Connection -> IO ()
initDb conn = SQL.execute_ conn
  "CREATE TABLE IF NOT EXISTS predictions(\
  \  id           INTEGER PRIMARY KEY,\
  \  content      TEXT NOT NULL,\
  \  created_at   TEXT NOT NULL,\
  \  due_at       TEXT NOT NULL,\
  \  user_id      TEXT NOT NULL,\
  \  user_name    TEXT NOT NULL,\
  \  guild_id     TEXT NOT NULL,\
  \  channel_id   TEXT NOT NULL,\
  \  notified     INTEGER NOT NULL DEFAULT 0) STRICT"

getUserData :: MemberOrUser -> Maybe (UserId, Username)
getUserData (
  MemberOrUser (
    Left (
      GuildMember
        { memberUser = Just (
            User { userId = uid, userName = un }
          )
        }
      )
    )
  ) = Just (uid, un)
getUserData _ = Nothing

parsePrediction :: UTCTime -> Interaction -> Either T.Text PredictionData
parsePrediction madeTime (cmd@InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} }) =
  do
    dataValues <- maybeToEither "Could not access the command's option fields." (optionsData input)
    predText <- maybeToEither "Could not access prediction field." . extractStringOption "prediction" $ dataValues
    dueDateText <- maybeToEither "Could not access date field." . extractStringOption "date" $ dataValues
    gid <- maybeToEither "Error accessing Guild ID. Command can only be used in a server." (interactionGuildId cmd)
    cid <- maybeToEither "Error accessing Channel ID." (interactionChannelId cmd)
    dueDateUTC <- maybeToEither
                 "Error parsing prediction date. Format is \"YYYY-MM-DD HH:MM:SS\""
                 (readMaybe . T.unpack $ dueDateText)
    (uid, un) <- maybeToEither
                   "Error accessing user data. Command can only be used in a server."
                   (getUserData . interactionUser $ cmd)
    return $
      PredictionData
      { predictionContent = predText
      , predictionMadeDate = madeTime
      , predictionDueDate = dueDateUTC
      , predictionUserId = uid
      , predictionUserName = un
      , predictionGuild = gid
      , predictionChannel = cid
      }
parsePrediction _ _ = Left "Tried to parse a non-prediction slash command."

savePrediction :: SQL.Connection -> PredictionData -> IO ()
savePrediction conn p = do
  SQL.execute conn "INSERT INTO predictions (content, created_at, due_at, user_id, user_name, guild_id, channel_id) VALUES (?,?,?,?,?,?,?)" p
