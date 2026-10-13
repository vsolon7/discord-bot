{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module JSONScheduler.Scheduler
  ( Scheduler(..)
  , Schedule(..)
  , module Data.Time
  , initScheduler
  , closeScheduler
  , nextJobTime
  , scheduleJob
  , runScheduler
  ) where

import JSONScheduler.Scheduler.Bell (Bell, await, ring, newBell)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO (putStrLn)
import qualified Database.SQLite.Simple as SQL
import Data.Aeson (ToJSON, FromJSON, eitherDecodeStrictText, encode)
import GHC.Generics (Generic)
import Control.Concurrent.MVar (MVar, withMVar, newMVar, takeMVar)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (forkIO)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Int (Int64)
import Control.Monad (forM, forM_, forever, void)
import System.Timeout (timeout)
import Data.Maybe (listToMaybe)
import Control.Monad.IO.Class (MonadIO, liftIO)

type DBConnection = MVar SQL.Connection

withDB :: DBConnection -> (SQL.Connection -> IO a) -> IO a
withDB = withMVar

newDBConnection :: SQL.Connection -> IO DBConnection
newDBConnection = newMVar

type JSONData = T.Text

data Scheduler a = Scheduler
  { schedDB :: DBConnection
  , schedBell :: Bell }

data Schedule
  = Once UTCTime
  | Every NominalDiffTime
  | Daily TimeOfDay
  | Weekly DayOfWeek TimeOfDay
  | Monthly DayOfMonth TimeOfDay
  | Yearly MonthOfYear DayOfMonth TimeOfDay
  | NthWeekday MonthOfYear DayOfWeek Int TimeOfDay
  | DateOnWeekday DayOfMonth DayOfWeek TimeOfDay
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

intToUTC :: Int64 -> UTCTime
intToUTC = posixSecondsToUTCTime . realToFrac

utcToIntFloor :: UTCTime -> Int64
utcToIntFloor = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

utcToIntCeil :: UTCTime -> Int64
utcToIntCeil = ceiling . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

ceilUTCTime :: UTCTime -> UTCTime
ceilUTCTime = intToUTC . utcToIntCeil

catRights :: [Either a b] -> [b]
catRights xs = [b | Right b <- xs]

decodeJSON :: FromJSON a => JSONData -> Either T.Text a
decodeJSON schedJSON = do
  case eitherDecodeStrictText schedJSON of
    Left err -> Left (T.pack err)
    Right x        -> Right x

encodeJSON :: ToJSON a => a -> T.Text
encodeJSON = TE.decodeUtf8 . BL.toStrict . encode

initScheduler :: FilePath -> IO (Scheduler a)
initScheduler databasePath = do
  conn <- SQL.open databasePath
  mapM_ (SQL.execute_ conn)
    [ "PRAGMA journal_mode = WAL"
    , "PRAGMA synchronous = NORMAL" ]
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS scheduled_jobs(\
    \  id        INTEGER PRIMARY KEY,\
    \  key       TEXT UNIQUE,\
    \  schedule  TEXT NOT NULL,\
    \  action    TEXT NOT NULL,\
    \  fire_at   INTEGER NOT NULL) STRICT"
  Scheduler <$> newDBConnection conn <*> newBell

closeScheduler :: Scheduler a -> IO ()
closeScheduler s = do
  c <- takeMVar (schedDB s)
  SQL.close c

-- TODO: better validation
validateSchedule :: Schedule -> Either T.Text Schedule
validateSchedule s = case s of
  Every d         | d < 1             -> Left "Tried to schedule a job to happen every r seconds, where r < 1."
  Monthly dom _   | dom < 1 || dom > 31 -> Left "Tried to schedule a monthly job on a day that never exists in any month."
  Yearly m dom _  | dom < 1 || dom > 31 || m < 1 || m > 12 -> Left "Tried to schedule a yearly job on a month or day that never exists."
  NthWeekday m _ n _   | m < 1 || m > 12 || n > 5 || n < -5 || n == 0 -> Left "Tried to schedule a job on the nth <day of week> of <month> each year, but the inputs given result in a date that never occurs."
  DateOnWeekday dom _ _    | dom < 1 || dom > 31 -> Left "Tried to schedule a job on a day that never exists in any month."
  _                                     -> Right s

-- cancelJob :: Scheduler a -> T.Text -> IO ()

nextOccurence :: UTCTime -> Schedule -> Maybe UTCTime
nextOccurence after sched = case sched of
  Once t -> if t > after then Just t else Nothing
  Every d -> Just (addUTCTime d after)
  Daily tod -> firstAfter [ LocalTime d tod | d <- [today..] ]
  Weekly dow tod -> firstAfter [ LocalTime d tod | d <- [today..], dayOfWeek d == dow ]
  Monthly dom tod ->
    let validFutureDates = [ v | Just v <- [ fromGregorianValid y m dom | (y, m) <- monthsFrom today ] ]
    in  firstAfter [ LocalTime v tod | v <- validFutureDates ]
  Yearly m dom tod ->
    let validFutureDates = [ v | Just v <- [ fromGregorianValid y m dom | y <- [thisYear..] ]]
    in  firstAfter [ LocalTime v tod | v <- validFutureDates ]
  NthWeekday m dow n tod -> firstAfter
    [ LocalTime d tod | y <- [thisYear..], Just d <- [nthWeekdayOfMonth y m dow n] ]
  DateOnWeekday dom dow tod -> firstAfter
    [ LocalTime d tod | (y, m) <- monthsFrom today, Just d <- [fromGregorianValid y m dom], dayOfWeek d == dow ]
  where
    today = utctDay after
    (thisYear,_,_) = toGregorian today
    firstAfter = listToMaybe . filter (> after) . map (localTimeToUTC utc)

monthsFrom :: Day -> [(Year, MonthOfYear)]
monthsFrom d = [ (y + fromIntegral q, r + 1) | k <- [0 ..], let (q, r) = (m - 1 + k) `divMod` 12 ]
  where (y, m, _) = toGregorian d

-- The nth weekday of a month: n >= 1 counts from the start, n <= -1 from the end.
-- Nothing if it doesn't exist (e.g. a 5th Thursday in a month that only has four).
-- Claude wrote this.
nthWeekdayOfMonth :: Year -> MonthOfYear -> DayOfWeek -> Int -> Maybe Day
nthWeekdayOfMonth y m dow n
  | n >= 1    = check (addDays (fromIntegral (fwd + 7 * (n - 1))) firstDay)
  | n <= -1   = check (addDays (negate (fromIntegral (back + 7 * (negate n - 1)))) lastDay)
  | otherwise = Nothing
  where
    firstDay = fromGregorian y m 1
    lastDay  = fromGregorian y m (gregorianMonthLength y m)
    fwd      = (fromEnum dow - fromEnum (dayOfWeek firstDay)) `mod` 7
    back     = (fromEnum (dayOfWeek lastDay) - fromEnum dow) `mod` 7
    check d  = let (_, m', _) = toGregorian d in if m' == m then Just d else Nothing

nextJobTime :: Scheduler a -> IO (Maybe UTCTime)
nextJobTime s = withDB (schedDB s) $
  \conn -> do
    res <- SQL.query_ conn q :: IO [SQL.Only (Maybe Int64)]
    case res of
      [SQL.Only (Just t)] -> return $ Just (intToUTC t)
      _                   -> return Nothing
  where
    q =
      "SELECT MIN(fire_at) \
      \FROM scheduled_jobs"

getDueJobs :: FromJSON a => Scheduler a -> UTCTime -> IO [a]
getDueJobs scheduler now = withDB (schedDB scheduler) $
  \conn -> SQL.withTransaction conn $ do
    -- | If t_j is the time the job is due and t_n is the current time, then fire_at = floor(t_j). Hence,
    -- we select all the jobs with floor(t_j) <= floor(t_n). This inequality is equivalent to floor(t_j) <= t_n,
    -- so jobs that weren't scheduled to occur at an integer number of seconds will fire up to 1 second early.
    rs <- SQL.query conn
      "SELECT id, schedule, action FROM scheduled_jobs WHERE fire_at <= ?"
      [utcToIntFloor now]
    fmap catRights . forM rs $ \(jid, schedJSON, actJSON) -> do
      let s = decodeJSON schedJSON
          ceilnow = ceilUTCTime now
      -- | For the reason above, we must check next the next occurence with the ceiling of the current time.
      -- We want to mark the job as finished, but I have to use my stupid comparison function if I want to keep the
      -- code "nice". It is true that floor(t_j) <= t_n, and under this assumption, this assumption itsef is equivalent
      -- to t_j <= ceil(t_n)
      case fmap (nextOccurence ceilnow) s of
        Left err -> do
          TIO.putStrLn $ "Error parsing the JSON data of the schedule. Error was:\n" <> err
          SQL.execute conn
            "DELETE FROM scheduled_jobs WHERE id = ?"
            [jid]
        Right mt -> case mt of
          Nothing -> do
            TIO.putStrLn $ "Deleted job with id " <> (T.show jid) <> " since there are no more scheduled occurences."
            SQL.execute conn
              "DELETE FROM scheduled_jobs WHERE id = ?"
              [jid]
          Just t -> do
            TIO.putStrLn $ "Updated job with id " <> (T.show jid) <> " to fire at " <> (T.show t) <> "."
            SQL.execute conn
              "UPDATE scheduled_jobs SET fire_at = ? WHERE id = ?"
              (utcToIntFloor t, jid :: Int64)
      return (decodeJSON actJSON)

scheduleJob :: ToJSON a => Scheduler a -> Maybe T.Text -> Schedule -> a -> IO (Either T.Text ())
scheduleJob scheduler key s action =
  case validateSchedule s of
    Left err -> return (Left err)
    Right valid -> do
      now <- getCurrentTime
      case nextOccurence now valid of
        Nothing -> return (Left "Tried to schedule a job that does not fire in the future.")
        Just t  -> do
          withDB (schedDB scheduler) $ \conn ->
            SQL.execute conn upsertJobQuery
              (key, encodeJSON valid, encodeJSON action, utcToIntFloor t)
          ring (schedBell scheduler)
          return (Right ())

upsertJobQuery :: SQL.Query
upsertJobQuery =
  "INSERT INTO scheduled_jobs (key, schedule, action, fire_at) \
  \VALUES (?, ?, ?, ?) \
  \ON CONFLICT(key) DO UPDATE SET \
  \  fire_at  = CASE WHEN schedule = excluded.schedule \
  \                 THEN fire_at ELSE excluded.fire_at END, \
  \  schedule = excluded.schedule, \
  \  action = excluded.action"

runScheduler :: (FromJSON a, Show a, MonadIO m, MonadUnliftIO m) => Scheduler a -> (a -> m ()) -> m ()
runScheduler s handler = void . forkIO . forever $ do
  now <- liftIO getCurrentTime
  due <- liftIO $ getDueJobs s now
  forM_ due $ \action -> do
    liftIO . TIO.putStrLn $ "Executed job:\n" <> (T.show action) <> "\n"
    handler action
  next <- liftIO $ nextJobTime s
  liftIO $ waitUntil (schedBell s) now next

waitUntil :: Bell -> UTCTime -> Maybe UTCTime -> IO ()
waitUntil bell _ Nothing = void . timeout (3600 * 1000000) $ await bell -- wait at most 1 hour before checking again
waitUntil bell now (Just t) = do
  let secs = min 3600 (t `diffUTCTime` now)
      micros = max 0 (ceiling (secs * 1000000)) :: Int
  void . timeout micros $ await bell -- wait max(1 hour, T), where T is the time until the next prediction
