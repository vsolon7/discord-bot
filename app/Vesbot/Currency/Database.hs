{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Currency.Database where

import Discord.Types
import Data.Typeable
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Text.Read (readMaybe)
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.Internal as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Data.Text as T

import Vesbot.Utils
import Vesbot.Database
import Vesbot.Parsing.Time
import Vesbot.Currency.Types


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
    \  num_messages     INTEGER NOT NULL DEFAULT 0,\
    \  num_reactions    INTEGER NOT NULL DEFAULT 0,\
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


updateReactionActivity :: DbConnection -> GuildId -> UserId -> UTCTime -> IO ()
updateReactionActivity dbconn gid uid now = withDb dbconn $
  \conn ->
    SQL.executeNamed conn
      "INSERT INTO activity (guild_id, user_id, last_reaction_at, num_reactions) VALUES (:gid, :uid, :now, 1)\
      \ON CONFLICT(guild_id, user_id)\
      \DO UPDATE SET last_reaction_at = :now, num_reactions = num_reactions + 1"
      [ ":now" SQL.:= (formatUTCTime now), ":uid" SQL.:= (showT uid), ":gid" SQL.:= (showT gid) ]


getLastCurrencyDrop dbconn = withDb dbconn $
  \conn -> do
    results <- SQL.query_ conn
      "SELECT drop_time FROM past_drop_info \
      \ORDER BY drop_number DESC \
      \LIMIT 1"
    case results of
      [] -> return (posixSecondsToUTCTime 0)
      [SQL.Only n] -> return (posixSecondsToUTCTime . fromInteger $ n)


updateTimeUntilNextDrop dbconn utctime = withDb dbconn $
  \conn -> do
    SQL.execute_ conn
      "DELETE FROM drop_meta"
    SQL.executeNamed conn
      "INSERT INTO drop_meta (next_drop_at) VALUES (:next)"
      [ ":next" SQL.:= (formatUTCTime utctime) ]


addToDropHistory dbconn gid uid amt time = withDb dbconn $
  \conn -> do
    SQL.executeNamed conn
      "INSERT INTO past_drop_info (drop_time, drop_amount, drop_winner_gid, drop_winner_uid) \
      \VALUES (:time, :amt, :gid, :uid)"
      [ ":time" SQL.:= (formatUTCTime time)
      , ":amt" SQL.:= amt
      , ":gid" SQL.:= showT gid
      , ":uid" SQL.:= showT uid ]


clearActivityData dbconn = withDb dbconn $
  \conn -> do
    SQL.execute_ conn
      "DELETE FROM activity"


selectEligibleUsers :: DbConnection -> GuildId -> UTCTime -> IO [RecentUserActivity]
selectEligibleUsers dbconn gid since = withDb dbconn $
  \conn -> do
    ruas <- SQL.queryNamed conn
      "SELECT user_id, num_messages, num_reactions FROM activity \
      \WHERE guild_id := gid \
      \AND (last_message_at > :lastdrop OR last_reaction_at > :lastdrop)"
      [ ":gid" SQL.:= showT gid, ":lastdrop" SQL.:= formatUTCTime since ] :: IO [RecentUserActivity]
    return ruas


selectCurrencyDropWinner :: DbConnection -> GuildId -> UTCTime -> IO (Maybe UserId)
selectCurrencyDropWinner dbconn gid since = withDb dbconn $
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
