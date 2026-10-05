{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Predictions where

import Discord
import Discord.Types
import Discord.Handle
import Discord.Internal.Rest.Channel
import qualified Discord.Requests as R
import Discord.Internal.Types.Interactions
import Text.Read (readMaybe)
import Utils (extractStringOption, extractIntegerOption, showT, echo)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.Internal as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import Data.Time.Clock (getCurrentTime, addUTCTime, diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Control.Monad.Reader (runReaderT)
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forever, forM_)
import System.Timeout
import Data.Int (Int64)
import Data.Word (Word64)
import Data.Typeable (Typeable)

type DbConnection = MVar SQL.Connection

withDb :: DbConnection -> (SQL.Connection -> IO a) -> IO a
withDb conn = withMVar conn

-- | Record to hold all of the information associated to the prediction slash command when the user
-- sends it to the bot
data PredictionCommandData = PredictionCommandData
  { predictionContent :: T.Text
  , predictionConfidence :: Maybe Integer
  , predictionMadeDate :: UTCTime
  , predictionDueDate :: UTCTime
  , predictionUserId :: UserId
  , predictionGuild :: GuildId
  , predictionChannel :: ChannelId
  } deriving Show

-- | This record adds the message ID of the bot's reply to the prediction slash command in the channel.
data PredictionData = PredictionData
  { predictionCommandData :: PredictionCommandData
  , predictionReplyMessageId :: MessageId
  } deriving Show

instance SQL.ToRow PredictionData where
  toRow (PredictionData pcd pmid) =
    [ SQL.SQLText (predictionContent pcd)
    , fromMaybe SQL.SQLNull (fmap (SQL.SQLInteger . fromInteger) (predictionConfidence pcd))
    , SQL.SQLInteger (formatUTC . predictionMadeDate $ pcd)
    , SQL.SQLInteger (formatUTC . predictionDueDate $ pcd)
    , SQL.SQLText (showT . predictionUserId $ pcd)
    , SQL.SQLText (showT . predictionGuild $ pcd)
    , SQL.SQLText (showT . predictionChannel $ pcd)
    , SQL.SQLText (showT pmid)
    ]
      where
        formatUTC t = fromInteger . ceiling . utcTimeToPOSIXSeconds $ t

instance SQL.FromRow PredictionData where
  fromRow = do
    cmdData <- PredictionCommandData
      <$> SQL.field
      <*> SQL.field
      <*> timeField
      <*> timeField
      <*> idField
      <*> idField
      <*> idField
    PredictionData cmdData <$> idField
      where
        timeField :: SQL.RowParser UTCTime
        timeField = posixSecondsToUTCTime . fromIntegral <$> (SQL.field :: SQL.RowParser Int64)
        idField :: Typeable a => SQL.RowParser (DiscordId a)
        idField = SQL.fieldWith $ \f -> do
          t <- SQL.fromField f :: SQL.Ok T.Text
          case readMaybe (T.unpack t) :: Maybe Word64 of
            Just w  -> pure (DiscordId (Snowflake w))
            Nothing -> SQL.returnError SQL.ConversionFailed f "not a valid snowflake"

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

initDb :: SQL.Connection -> IO ()
initDb conn = SQL.execute_ conn
  "CREATE TABLE IF NOT EXISTS predictions(\
  \  id             INTEGER PRIMARY KEY,\
  \  content        TEXT NOT NULL,\
  \  confidence     INTEGER,\
  \  created_at     INTEGER NOT NULL,\
  \  due_at         INTEGER NOT NULL,\
  \  user_id        TEXT NOT NULL,\
  \  guild_id       TEXT NOT NULL,\
  \  channel_id     TEXT NOT NULL,\
  \  reply_mess_id  TEXT NOT NULL,\
  \  notified       INTEGER NOT NULL DEFAULT 0) STRICT"

getUserData :: MemberOrUser -> Maybe UserId
getUserData (
  MemberOrUser (
    Left (
      GuildMember
        { memberUser = Just (
            User { userId = uid }
          )
        }
      )
    )
  ) = Just uid

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
    let confidence = extractIntegerOption "confidence" dataValues
    predText <- maybeToEither "Could not access prediction field." . extractStringOption "prediction" $ dataValues
    dueDateText <- maybeToEither "Could not access date field." . extractStringOption "date" $ dataValues
    gid <- maybeToEither "Error accessing Guild ID. Note that this command can only be used in a server." (interactionGuildId cmd)
    cid <- maybeToEither "Error accessing Channel ID." (interactionChannelId cmd)
    uid <- maybeToEither
             "Could not get user data. Note that this command can only be used in a server."
             (getUserData . interactionUser $ cmd)
    dueDateUTC <- maybeToEither
                    "Error parsing prediction date. Here are the formatting options.\n\
                   \ **Relative time:** \"in #w\" or \"in #d\" or \"in #h\" or \"in #m\", where # is a positive integer. \"w\" stands for weeks, \"d\" stands for days, \"h\" stands for hours, and \"m\" stands for minutes.\n\
                   \ **Absolute time:** \"on month-day-year HH:MM <timezone>\", where the timezone is optional. If no timezone is entered, the bot will default to UTC. Timezone examples are CST, CDT, EST, etc."
                    (parsePredictionDateInput currTime . T.unpack $ dueDateText)
    return $
      PredictionCommandData
        { predictionContent = predText
        , predictionConfidence = confidence
        , predictionMadeDate = currTime
        , predictionDueDate = dueDateUTC
        , predictionUserId = uid
        , predictionGuild = gid
        , predictionChannel = cid
        }

parsePredictionCommand _ _ = Left "Tried to parse a non-prediction slash command."


savePrediction :: DbConnection -> MVar () -> PredictionData -> IO ()
savePrediction dbconn wake p = withDb dbconn $ \conn -> do
  SQL.execute conn "INSERT INTO predictions\
                   \ (content, confidence, created_at, due_at, user_id, guild_id, channel_id, reply_mess_id)\
                   \VALUES (?,?,?,?,?,?,?,?)" p
  void (tryPutMVar wake ())



makeMessageSender :: DiscordHandle -> (ChannelRequest Message -> IO ())
makeMessageSender h messageReq = do
    result <- runReaderT (restCall messageReq) h
    case result of
      Left err -> echo $ "Message sender failed to send message. Discord returned the error:\n" <> showT err
      Right _  -> return ()


nextPredictionTime :: DbConnection -> IO (Maybe UTCTime)
nextPredictionTime dbconn = withDb dbconn $ \conn -> do
  nextList <- SQL.query_ conn
                    "SELECT MIN(due_at)\
                   \ FROM predictions\
                   \ WHERE notified = 0" :: IO [SQL.Only (Maybe Integer)]
  case nextList of
    [SQL.Only (Just t)] -> return $ Just (posixSecondsToUTCTime . fromInteger $ t)
    _                   -> return Nothing


duePredictions :: DbConnection -> IO [PredictionData]
duePredictions dbconn = withDb dbconn $ \conn -> do
  nowPOSIX <- fromIntegral . ceiling . utcTimeToPOSIXSeconds <$> getCurrentTime :: IO Integer
  duePreds <- SQL.query conn
                "SELECT content, confidence, created_at, due_at, user_id, guild_id, channel_id, reply_mess_id\
               \ FROM predictions\
               \ WHERE notified = 0 AND due_at <= ?"
               (SQL.Only nowPOSIX) :: IO [PredictionData]

  SQL.execute conn
    "UPDATE predictions SET notified = 1 \
    \WHERE notified = 0 AND due_at <= ?"
    (SQL.Only nowPOSIX)

  return duePreds


createPredictionAnnouncement :: PredictionData -> ChannelRequest Message
createPredictionAnnouncement (PredictionData p mid) =
  let
    userPing = "<@" <> showT (predictionUserId p) <> ">"
    predDateUTC = showT . ceiling . utcTimeToPOSIXSeconds $ predictionMadeDate p
    predDateTimeStampRel = "<t:" <> predDateUTC <> ":R>"
    predDateTimeStampAbs = "<t:" <> predDateUTC <> ":f>"
    claim = case predictionConfidence p of
                      Nothing -> "claimed"
                      Just c  -> "was " <> showT c <> "%" <> " confident"
    content =
      predDateTimeStampRel <> ", on " <> predDateTimeStampAbs <> ", " <> userPing <> " " <> claim <>
      " that on or before this time today, the following would happen:\n" <> "*" <>
      (predictionContent p) <> "*.\n" <>
      "Were they correct?"
    mref =
      Just $
        MessageReference
          (Just mid)
          (Just (predictionChannel p))
          (Just (predictionGuild p))
          True
    in
      R.CreateMessageDetailed
        (predictionChannel p)
        (MessageDetailedOpts
          content
          False
          Nothing
          Nothing
          Nothing
          mref
          Nothing
          Nothing)


startNotifier :: DbConnection -> (ChannelRequest Message -> IO ()) -> MVar () -> IO ()
startNotifier dbconn sender wake = void . forkIO . forever $ do
  ps <- duePredictions dbconn
  forM_ ps $ \p -> do
    sender (createPredictionAnnouncement p)
    threadDelay (5 * 1000000) -- wait 5 seconds between announcements for predictions that are close to each other

  npt <- nextPredictionTime dbconn
  now <- getCurrentTime

  case npt of
    Nothing -> takeMVar wake
    Just t  -> do
      let until = min 3600 (diffUTCTime t now) -- rest for at most 1 hour before checking again
          restFor = max 0 (ceiling (until * 1000000)) :: Int
      void (timeout restFor (takeMVar wake))

