{-# LANGUAGE OverloadedStrings #-}
module Currency where

import Discord
import Discord.Types
import Discord.Handle
import Discord.Handle
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Interactions
import qualified Discord.Requests as R
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.Internal as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Data.Text as T
import Control.Concurrent (forkIO)
import Control.Monad (void, forM)
import Predictions
import Utils (showT, extractNumberOption, extractUserOption, echo, ephemeralResponseBasic)
import UnliftIO (liftIO, Typeable)
import Control.Monad.Reader (ask, runReaderT)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

data CurrencyUpdate = CurrencyUpdate
  { currencyUpdateUser :: UserId
  , currencyUpdateAmount :: Double
  } deriving Show

data CurrencyQuery = CurrencyQuery
  { currencyQueryUser :: UserId
  , currencyQueryAmount :: Double
  } deriving Show

data PayCommandData = PayCommandData
  { payFromUser :: UserId
  , payToUser :: UserId
  , payAmount :: Double
  } deriving Show

data ViewCurrencyCommandData = ViewCurrencyCommandData
  { viewCurrencyUser :: UserId
  } deriving Show

data RecentUserActivity = RecentUserActivity
  { recentActivityUserId :: UserId
  , recentActivityMessages :: Maybe Integer
  , recentActivityReacts :: Maybe Integer
  } deriving Show

data CurrencyDropResults = CurrencyDropResults
  { currencyDropWinner :: Maybe CurrencyUpdate
  , currencyDropEquity :: [CurrencyUpdate]
  } deriving Show

instance SQL.FromRow RecentUserActivity where
  fromRow = RecentUserActivity <$> idField <*> SQL.field <*> SQL.field
    where
      idField :: Typeable a => SQL.RowParser (DiscordId a)
      idField = SQL.fieldWith $ \f -> do
        t <- SQL.fromField f :: SQL.Ok T.Text
        case readMaybe (T.unpack t) :: Maybe Word64 of
          Just w  -> pure (DiscordId (Snowflake w))
          Nothing -> SQL.returnError SQL.ConversionFailed f "not a valid snowflake"


initCurrencyTable :: SQL.Connection -> IO ()
initCurrencyTable conn =
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS currency(\
    \  user_id       TEXT PRIMARY KEY,\
    \  currency_amt  REAL NOT NULL) STRICT"


initActivityTable :: SQL.Connection -> IO ()
initActivityTable conn =
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS activity(\
    \  guild_id         TEXT NOT NULL,\
    \  user_id          TEXT NOT NULL,\
    \  last_message_at  INTEGER,\
    \  last_reaction_at INTEGER,\
    \  num_messages     INTEGER,\
    \  num_reactions    INTEGER,\
    \  PRIMARY KEY      (guild_id, user_id)) STRICT"


initPastCurrencyDropInfoTable :: SQL.Connection -> IO ()
initPastCurrencyDropInfoTable conn =
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS past_drop_info(\
    \  drop_number     INTEGER PRIMARY KEY,\
    \  drop_time       INTEGER NOT NULL,\
    \  drop_amount     INTEGER NOT NULL,\
    \  drop_winner_uid TEXT NOT NULL,\
    \  drop_winner_gid TEXT NOT NULL) STRICT"


initCurrencyDropMetaTable :: SQL.Connection -> IO ()
initCurrencyDropMetaTable conn = do
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS drop_meta(\
    \  next_drop_at   INTEGER PRIMARY KEY) STRICT"


updateCurrency :: DbConnection -> CurrencyUpdate -> IO ()
updateCurrency dbconn cu = withDb dbconn $
  \conn -> do
    SQL.executeNamed conn
      "INSERT INTO currency (user_id, currency_amt) VALUES (:userid, :change)\
      \ON CONFLICT(user_id)\
      \DO UPDATE SET currency_amt = currency_amt + :change"
      [ ":change" SQL.:= (currencyUpdateAmount cu), ":userid" SQL.:= (showT . currencyUpdateUser $ cu) ]


getCurrency :: DbConnection -> UserId -> IO CurrencyQuery
getCurrency dbconn uid = withDb dbconn $
  \conn -> do
    res <-
      SQL.queryNamed conn
        "SELECT currency_amt FROM currency WHERE user_id = :userid"
        [ ":userid" SQL.:= (showT uid) ] :: IO [SQL.Only (Maybe Double)]
    case res of
      [SQL.Only (Just amt)] -> return (CurrencyQuery uid amt)
      _                     -> return (CurrencyQuery uid 0)


parsePayCommand :: Interaction -> Either T.Text PayCommandData
parsePayCommand (cmd@InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} }) = do
  dataValues <-
    maybeToEither
      "Could not access the pay command's option fields."
      (optionsData input)
  payTo <-
    maybeToEither
      "Could not access \"pay to\" field."
      (extractUserOption "pay_to" dataValues)
  payAmount <-
    maybeToEither
      "Could not access payment amount field."
      (extractNumberOption "amount" dataValues)
  uid <-
    maybeToEither
      "Could not get user data. Note that this command can only be used in a server."
      (getUserData . interactionUser $ cmd)
  return $
    PayCommandData
      { payFromUser = uid
      , payToUser = payTo
      , payAmount = payAmount }

parsePayCommand _ = Left "Tried to parse a non slash command when parsing the pay command."


createPaymentResponse :: DbConnection -> Either T.Text PayCommandData -> IO InteractionResponse
createPaymentResponse dbconn pcd =
  case pcd of
    Right p -> do
      CurrencyQuery { currencyQueryAmount = userCurrency } <- getCurrency dbconn (payFromUser p)
      response <- do
        if userCurrency >= payAmount p then
          let response =
                "Paid " <> showT (payAmount p) <> " currency units to <@" <> showT (payToUser p) <> ">."
              remove = CurrencyUpdate (payFromUser p) ((-1) * (payAmount p))
              add = CurrencyUpdate (payToUser p) (payAmount p)
          in do
            void . forkIO $ do -- multithread the database updates so that we can send a response sooner
              updateCurrency dbconn remove
              updateCurrency dbconn add
            return response
        else do
          return "You don't have enough currency!"
      return (ephemeralResponseBasic response)
    Left err -> return (ephemeralResponseBasic err)


parseViewCurrencyCommand :: Interaction -> Either T.Text ViewCurrencyCommandData
parseViewCurrencyCommand (cmd@InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} }) = do
  dataValues <-
    maybeToEither
      "Could not access the view currency command's option fields."
      (optionsData input)
  user <-
    maybeToEither
      "Could not access user field in the view currency command."
      (extractUserOption "user" dataValues)
  return $
    ViewCurrencyCommandData user

parseViewCurrencyCommand _ = Left "Tried to parse a non slash command when parsing the view currency command."


createViewCurrencyResponse :: DbConnection -> Either T.Text ViewCurrencyCommandData -> IO InteractionResponse
createViewCurrencyResponse dbconn vccd =
  case vccd of
  Right v -> do
    c <- getCurrency dbconn (viewCurrencyUser v)
    let userPing = "<@" <> showT (viewCurrencyUser v) <> ">"
        response = userPing <> " has " <> showT (currencyQueryAmount c) <> " units of currency."
    return (ephemeralResponseBasic response)
  Left err -> return (ephemeralResponseBasic err)


updateReactionActivity :: DbConnection -> GuildId -> UserId -> UTCTime -> IO ()
updateReactionActivity dbconn gid uid now = withDb dbconn $
  \conn ->
    SQL.executeNamed conn
      "INSERT INTO activity (guild_id, user_id, last_reaction_at, num_reactions) VALUES (:gid, :uid, :now, 1)\
      \ON CONFLICT(guild_id, user_id)\
      \DO UPDATE SET last_reaction_at = :now, num_reactions = num_reactions + 1"
      [ ":now" SQL.:= (formatUTCTime now), ":uid" SQL.:= (showT uid), ":gid" SQL.:= (showT gid) ]
  where
    formatUTCTime = toInteger . floor . utcTimeToPOSIXSeconds


updateMessageActivity :: DbConnection -> GuildId -> UserId -> UTCTime -> IO ()
updateMessageActivity dbconn gid uid now = withDb dbconn $
  \conn ->
    SQL.executeNamed conn
      "INSERT INTO activity (guild_id, user_id, last_message_at, num_messages) VALUES (:gid, :uid, :now, 1)\
      \ON CONFLICT(guild_id, user_id)\
      \DO UPDATE SET last_message_at = :now, num_messages = num_messages + 1"
      [ ":now" SQL.:= (formatUTCTime now), ":uid" SQL.:= (showT uid), ":gid" SQL.:= (showT gid) ]
  where
    formatUTCTime = toInteger . floor . utcTimeToPOSIXSeconds


reactionHandler :: DbConnection -> ReactionInfo -> DiscordHandler ()
reactionHandler dbconn reactInfo = do
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
                case messageGuildId message of
                  Nothing -> echo $ "Failed to get guild ID of user when they reacted."
                  Just gid -> updateReactionActivity dbconn gid (userId op) now


createCurrencyDropMessage :: ChannelId -> CurrencyDropResults -> ChannelRequest Message
createCurrencyDropMessage cid (CurrencyDropResults winnerChange otherChanges) =
  let winnerStatement =
        case winnerChange of
          Nothing -> "There was no winner!"
          Just wc -> "Winner: <@" <> (showT . currencyUpdateUser $ wc) <> ">: +" <> (showT . currencyUpdateAmount $ wc)
      otherUpdate c = "<@" <> (showT . currencyUpdateUser $ c) <> ">: +" <> (showT . currencyUpdateAmount $ c)
      otherUpdateStatement = T.concat [otherUpdate c <> "\n" | c <- otherChanges]
      content =
        "A currency drop happened!\n" <> winnerStatement <> "\n" <>
        "Users with equity in the drop:" <> otherUpdateStatement
  in
    R.CreateMessage
      cid
      content


payCurrencyDropWinner :: DbConnection -> UTCTime -> GuildId -> Double -> IO (Maybe CurrencyUpdate)
payCurrencyDropWinner dbconn since gid jackpot = do
  winner <- selectCurrencyDropWinner dbconn gid
  case winner of
    Nothing  -> return Nothing
    Just wid -> do
      let update = CurrencyUpdate wid jackpot
      updateCurrency dbconn update
      return $ Just update
    where
      formatUTCTime = toInteger . floor . utcTimeToPOSIXSeconds
      selectCurrencyDropWinner :: DbConnection -> GuildId -> IO (Maybe UserId)
      selectCurrencyDropWinner dbconn gid = withDb dbconn $
        \conn -> do
          winner <-
            SQL.queryNamed conn
              "SELECT user_id FROM activity \
              \WHERE guild_id = :gid \
              \AND (last_message_at > :lastdrop OR last_reaction_at > :lastdrop) \
              \ORDER BY RANDOM() LIMIT 1"
              [ ":gid" SQL.:= showT gid, ":lastdrop" SQL.:= formatUTCTime since ] :: IO [SQL.Only T.Text]
          case winner of
            [SQL.Only wid] -> return (read . T.unpack $ wid)
            _              -> return Nothing


computeCurrencyDropEquity :: [RecentUserActivity] -> [(UserId, Double)]
computeCurrencyDropEquity rs =
  let totalMessages = sum . map (fromMaybe 0 . recentActivityMessages) $ rs
      totalReacts = sum . map (fromMaybe 0 . recentActivityReacts) $ rs
      equity :: Integer -> Integer -> Double -> Double -> RecentUserActivity -> (UserId, Double)
      equity totM totR mWeight rWeight r =
        let mProp = (fromIntegral (fromMaybe 0 . recentActivityMessages $ r) / fromIntegral totM)
            rProp = (fromIntegral (fromMaybe 0 . recentActivityMessages $ r) / fromIntegral totM)
        in (recentActivityUserId r, mWeight * mProp + rWeight * rProp)
  in map (equity totalMessages totalReacts 0.10 0.90) rs


payCurrencyDropEquity :: DbConnection -> UTCTime -> GuildId -> Double -> IO [CurrencyUpdate]
payCurrencyDropEquity dbconn since gid dropamt = do
  ruas <- selectEligibleUsers dbconn gid
  forM (computeCurrencyDropEquity ruas) $ \(uid, e) -> do
    let update = CurrencyUpdate uid (e * dropamt)
    updateCurrency dbconn update
    return update
  where
    formatUTCTime = toInteger . floor . utcTimeToPOSIXSeconds
    selectEligibleUsers :: DbConnection -> GuildId -> IO [RecentUserActivity]
    selectEligibleUsers dbconn gid = withDb dbconn $
      \conn -> do
        ruas <- SQL.queryNamed conn
          "SELECT user_id, num_messages, num_reactions FROM activity \
          \WHERE guild_id := gid \
          \AND (last_message_at > :lastdrop OR last_reaction_at > :lastdrop)"
          [ ":gid" SQL.:= showT gid, ":lastdrop" SQL.:= formatUTCTime since ] :: IO [RecentUserActivity]
        return ruas


doCurrencyDrop :: DbConnection -> UTCTime -> GuildId -> Double -> IO CurrencyDropResults
doCurrencyDrop dbconn since gid dropTotal = do
  winnerUpdate <- payCurrencyDropWinner dbconn since gid (0.3 * dropTotal)
  equityUpdates <- payCurrencyDropEquity dbconn since gid (0.7 * dropTotal)
  return $ CurrencyDropResults winnerUpdate equityUpdates

