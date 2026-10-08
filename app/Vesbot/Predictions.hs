{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Vesbot.Predictions (
  module Vesbot.Predictions,
  module Vesbot.Predictions.Commands,
  module Vesbot.Predictions.Database,
  module Vesbot.Predictions.Types
) where

import Discord
import Discord.Types
import Discord.Handle
import Discord.Internal.Rest.Channel
import qualified Discord.Requests as R
import Discord.Internal.Types.Interactions
import Text.Read (readMaybe)
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
import Control.Monad.Reader (runReaderT, ask, liftIO)
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forever, forM_)
import System.Timeout
import Data.Int (Int64)
import Data.Word (Word64)
import Data.Typeable (Typeable)

import Vesbot.Database
import Vesbot.Utils (extractStringOption, extractIntegerOption, showT, echo, ephemeralResponseBasic)
import Vesbot.Predictions.Types
import Vesbot.Predictions.Database
import Vesbot.Predictions.Commands


 -- | This function starts the prediction notifier process on a new thread.
 -- The notifier rests by trying to take from an empty MVar. If any other process or function wants to
 -- wake the notifier (for example, when a new prediction is made), it should put () into this same MVar.
startNotifier :: DbConnection -> MVar () -> DiscordHandler ()
startNotifier dbconn wake = do
  h <- ask  -- grab the connection so that we can use it to fork a new IO thread that can make restCalls
  void . liftIO . forkIO . forever $ do
    -- Get the predictions that are due (or past due)
    ps <- duePredictions dbconn

    forM_ ps $ \p -> do
      -- Send the prediction notification message
      res <- runReaderT (restCall (createPredictionAnnouncement p)) h
      case res of
        Left err -> do
          echo $
            "Failed to send prediction result announcemnt.\
            \Prediction data was:\n" <> showT p <>
            "Discord returned the error:\n" <> showT err
        Right _  -> return ()
      threadDelay (3 * 1000000) -- wait 3 seconds between announcements for predictions that are close to each other

    -- checks the time T until the next prediction due date and rests for min(T, 1 hour).
    npt <- nextPredictionTime dbconn
    now <- getCurrentTime

    case npt of
      Nothing -> takeMVar wake
      Just t  -> do
        let until = min 3600 (diffUTCTime t now) -- rest for at most 1 hour before checking again
            restFor = max 0 (ceiling (until * 1000000)) :: Int
        void (timeout restFor (takeMVar wake))

