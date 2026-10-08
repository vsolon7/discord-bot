{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Currency (
  module Vesbot.Currency.Commands,
  module Vesbot.Currency.Types,
  module Vesbot.Currency.Database
) where

import Vesbot.Currency.Types
import Vesbot.Currency.Database
import Vesbot.Currency.Commands
import Discord
import Discord.Types
import Discord.Handle
import Discord.Handle
import Discord.Internal.Rest.Channel
import qualified Data.Text as T
import Control.Concurrent (forkIO)
import Control.Monad (void, forM)
import UnliftIO (liftIO, Typeable)
import Control.Monad.Reader (ask, runReaderT)
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Word
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import Vesbot.Predictions
import Vesbot.Utils
import Vesbot.Database


payCurrencyDropWinner :: DbConnection -> UTCTime -> GuildId -> Double -> IO (Maybe CurrencyUpdate)
payCurrencyDropWinner dbconn since gid jackpot = do
  winner <- selectCurrencyDropWinner dbconn gid since
  case winner of
    Nothing  -> return Nothing
    Just wid -> do
      let update = CurrencyUpdate wid jackpot
      updateCurrency dbconn update
      return $ Just update


computeCurrencyDropEquity :: [RecentUserActivity] -> [(UserId, Double)]
computeCurrencyDropEquity rs =
  let totalMessages = sum . map recentActivityMessages $ rs
      totalReacts = sum . map recentActivityReacts $ rs
      equity :: Integer -> Integer -> Double -> Double -> RecentUserActivity -> (UserId, Double)
      equity totM totR mWeight rWeight r =
        let mProp = (fromIntegral (recentActivityMessages r) / fromIntegral totM)
            rProp = (fromIntegral (recentActivityMessages r) / fromIntegral totM)
        in (recentActivityUserId r, mWeight * mProp + rWeight * rProp)
  in map (equity totalMessages totalReacts 0.10 0.90) rs


payCurrencyDropEquity :: DbConnection -> UTCTime -> GuildId -> Double -> IO [CurrencyUpdate]
payCurrencyDropEquity dbconn since gid dropamt = do
  ruas <- selectEligibleUsers dbconn gid since
  forM (computeCurrencyDropEquity ruas) $ \(uid, e) -> do
    let update = CurrencyUpdate uid (e * dropamt)
    updateCurrency dbconn update
    return update


doCurrencyDropUpdates :: DbConnection -> UTCTime -> GuildId -> Double -> IO CurrencyDropResults
doCurrencyDropUpdates dbconn since gid dropTotal = do
  winnerUpdate <- payCurrencyDropWinner dbconn since gid (0.3 * dropTotal)
  equityUpdates <- payCurrencyDropEquity dbconn since gid (0.7 * dropTotal)
  return $ CurrencyDropResults winnerUpdate equityUpdates


doCurrencyDrop :: DbConnection -> GuildId -> ChannelId -> Double -> DiscordHandler UTCTime
doCurrencyDrop dbconn gid cid dropTotal = do
  lastDrop <- liftIO $ getLastCurrencyDrop dbconn
  now <- liftIO getCurrentTime
  results <- liftIO $ doCurrencyDropUpdates dbconn lastDrop gid dropTotal
  let message = createCurrencyDropMessage cid results
  
  restCallRes <- restCall message
  case restCallRes of
    Left err -> echo $ "Error sending the currency drop results message. Discord returned the error:\n" <> showT err
    Right _  -> return ()

  let timeUntilNextDrop = fromInteger (24 * 3600) :: NominalDiffTime --24h between drops
  let nextDropTime = addUTCTime timeUntilNextDrop now

  liftIO $ do
    updateTimeUntilNextDrop dbconn nextDropTime
    case (currencyDropWinner results) of
      Nothing  -> return ()
      Just wud -> addToDropHistory dbconn gid (currencyUpdateUser wud) (currencyUpdateAmount wud) now
    clearActivityData dbconn
  return nextDropTime

