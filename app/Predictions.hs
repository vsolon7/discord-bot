{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Predictions where

import Discord.Types
import Discord.Internal.Types.Interactions
import Text.Read (readMaybe)
import Utils (extractStringOption, showT)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Data.Time.Format (defaultTimeLocale, parseTimeMultipleM, parseTimeM)
import Data.Time.Clock (addUTCTime, NominalDiffTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)

type Username = T.Text

-- | Record to hold all of the information associated to the prediction slash command when the user
-- sends it to the bot
data PredictionCommandData = PredictionCommandData
  { predictionContent :: T.Text
  , predictionMadeDate :: UTCTime
  , predictionDueDate :: UTCTime
  , predictionUserId :: UserId
  , predictionUserName :: Username
  , predictionGuild :: GuildId
  , predictionChannel :: ChannelId
  } deriving Show

-- | This record adds the message ID of the bot's reply to the prediction slash command in the channel.
data PredictionData = PredictionData
  { predictionCommandData :: PredictionCommandData
  , predictionReplyMessageId :: MessageId
  }

instance SQL.ToRow PredictionData where
  toRow (PredictionData pcd pmid) =
    [ SQL.SQLText (predictionContent pcd)
    , SQL.SQLText (showT . predictionMadeDate $ pcd)
    , SQL.SQLText (showT . predictionDueDate $ pcd)
    , SQL.SQLText (showT . predictionUserId $ pcd)
    , SQL.SQLText (predictionUserName pcd)
    , SQL.SQLText (showT . predictionGuild $ pcd)
    , SQL.SQLText (showT . predictionChannel $ pcd)
    , SQL.SQLText (showT pmid)
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
  \  reply_mid    TEXT NOT NULL,\
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

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x:_) = x

parsePredictionDateInput :: UTCTime -> String -> Maybe UTCTime
parsePredictionDateInput currTime inpt = case words inpt of
  ("on":absInpt:timeInpt:timeZoneOrEmpty) ->
    let
      timeZone = parseTimeM False defaultTimeLocale "%Z" (safeHead "UTC" timeZoneOrEmpty)
    in do
      date <- parseTimeM False defaultTimeLocale "%m-%d-%Y" absInpt :: Maybe Day
      time <- parseTimeM False defaultTimeLocale "%R" timeInpt :: Maybe TimeOfDay
      tz <- timeZone
      return (localTimeToUTC tz (LocalTime date time))

  ("in":relInpt:_) -> do
    let letter = dropWhile isDigit relInpt
    timeIncrement <- parseTimeM False defaultTimeLocale (concat ["%", letter, letter]) relInpt :: Maybe NominalDiffTime
    return (addUTCTime timeIncrement currTime)

  _          -> Nothing

parsePredictionCommand :: UTCTime -> Interaction -> Either T.Text PredictionCommandData
parsePredictionCommand currTime (cmd@InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} }) =
  do
    dataValues <- maybeToEither "Could not access the command's option fields." (optionsData input)
    predText <- maybeToEither "Could not access prediction field." . extractStringOption "prediction" $ dataValues
    dueDateText <- maybeToEither "Could not access date field." . extractStringOption "date" $ dataValues
    gid <- maybeToEither "Error accessing Guild ID. Note that this command can only be used in a server." (interactionGuildId cmd)
    cid <- maybeToEither "Error accessing Channel ID." (interactionChannelId cmd)
    (uid, un) <- maybeToEither
                   "Could not get user data. Note that this command can only be used in a server."
                   (getUserData . interactionUser $ cmd)
    dueDateUTC <- maybeToEither
                    "Error parsing prediction date. Here are the formatting options.\n\
                   \ **Relative time:** \"in #w\" or \"in #d\" or \"in #h\" or \"in #m\", where # is a positive integer. \"w\" stands for weeks, \"d\" stands for days, \"h\" stands for hours, and \"m\" stands for minutes.\n\
                   \ **Absolute time:** \"on month-day-year HH:MM [timezone]\", where [] means that field is optional. If no timezone is entered, the bot will default to UTC. Timezone examples are CST, CDT, EST, etc."
                    (parsePredictionDateInput currTime . T.unpack $ dueDateText)
    return $
      PredictionCommandData
        { predictionContent = predText
        , predictionMadeDate = currTime
        , predictionDueDate = dueDateUTC
        , predictionUserId = uid
        , predictionUserName = un
        , predictionGuild = gid
        , predictionChannel = cid
        }

parsePredictionCommand _ _ = Left "Tried to parse a non-prediction slash command."

savePrediction :: SQL.Connection -> PredictionData -> IO ()
savePrediction conn p = do
  SQL.execute conn "INSERT INTO predictions (content, created_at, due_at, user_id, user_name, guild_id, channel_id, reply_mid) VALUES (?,?,?,?,?,?,?,?)" p
