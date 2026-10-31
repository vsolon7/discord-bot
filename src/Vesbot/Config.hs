{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Vesbot.Config (
  Env (..),
  Config (..),
  CurrencyConfig (..),
  DropTableRates (..),
  initBot,
) where

import JSONScheduler.Scheduler as Scheduler (Schedule (..), Scheduler (..), initScheduler, scheduleJob)
import Vesbot.Actions (Action (..))
import Vesbot.Database as Database (initDBConnection)
import Vesbot.Database.Types (DBConnection)
import Vesbot.Logging as Logging (LogType (..), log)
import Vesbot.Parsing (parseJSON)
import Vesbot.Responses as Responses (KeywordResponse, RareEmoji, RareEmojiData (..), createRareEmoji, initKeywordResponses)
import Vesbot.Utils (forM_, traverse_)

import Discord.Types

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Text.Read (readMaybe)

--TODO: asdfasdf
data CurrencyConfig = CurrencyConfig
  { currencyName :: T.Text
  , currencySymbolKilo :: T.Text
  , currencySymbolNormal :: T.Text
  , currencySymbolMilli :: T.Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype DropTableRates = DropTableRates (Int -> Int) -- generator function for the chance for 1 drop, 2 drops, 3 drops, etc.

data Config = Config
  { cfgGuildId :: GuildId
  , cfgResponses :: [KeywordResponse]
  , cfgAnnouncementCID :: ChannelId
  , cfgCurrencyInfo :: CurrencyConfig
  , cfgRareEmoji :: [RareEmoji]
  , cfgDropTableRates :: DropTableRates
  }

data Env = Env
  { envDBConnection :: DBConnection
  , envScheduler :: Scheduler Action
  }

type ActionId = T.Text
data InitialAction = InitialAction ActionId Action Schedule
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

_DATABASE_FILE :: FilePath
_DATABASE_FILE = "appdata/database/data.db"
_SCHEDULER_DATABASE_FILE :: FilePath
_SCHEDULER_DATABASE_FILE = "appdata/scheduler/scheduler.db"
_RESPONSES_FILE :: FilePath
_RESPONSES_FILE = "appdata/config/responses.json"
_INITIAL_ACTIONS_FILE :: FilePath
_INITIAL_ACTIONS_FILE = "appdata/config/initial_actions.json"
_ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE :: FilePath
_ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE = "appdata/config/announcement_channel_id"
_CURRENCY_CONFIG_FILE :: FilePath
_CURRENCY_CONFIG_FILE = "appdata/config/currency_info.json"
_RARE_EMOJI_FILE :: FilePath
_RARE_EMOJI_FILE = "appdata/config/rare_emojis.json"
_DROP_TABLE_RATES :: Int -> Int
_DROP_TABLE_RATES n
  | n == 0 = 650
  | 0 < n && n < 4 = 2
  | 4 <= n = 3
  | otherwise = 0

getToken :: IO T.Text
getToken = TIO.readFile "../discord-bot-apidata/token"

getGuildId :: IO GuildId
getGuildId = do
  gids <- readFile "../discord-bot-apidata/guildid"
  case readMaybe gids of
    Just g -> return g
    Nothing -> do
      Logging.log Error "Could not parse Guild ID from discord-bot-apidata/guildid"
      error "fatal error"

initBot :: IO (T.Text, Env, Config)
initBot = do
  t <- getToken
  gid <- getGuildId

  s <- Scheduler.initScheduler _SCHEDULER_DATABASE_FILE :: IO (Scheduler Action)
  krs <- Responses.initKeywordResponses _RESPONSES_FILE
  conn <- Database.initDBConnection _DATABASE_FILE

  hs <- parseJSON _INITIAL_ACTIONS_FILE :: IO [InitialAction]
  scheduleJSONActions hs s

  acid <- readAnnouncementChannelId _ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE

  cc <- parseJSON _CURRENCY_CONFIG_FILE :: IO CurrencyConfig
  reds <- parseJSON _RARE_EMOJI_FILE :: IO [RareEmojiData]

  let validateRareEmoji red
        | rareEmojiDataChance red < 0 = do
            Logging.log Error "Rare emoji chance sent to <0."
        | rareEmojiDataChance red == 0 = do
            Logging.log Warn "Rare emoji chance set to 0. This means that this reaction is removed from the drop table."
        | otherwise = return ()

  traverse_ validateRareEmoji reds
  let res = map createRareEmoji reds

  let rates = DropTableRates _DROP_TABLE_RATES

  return (t, Env conn s, Config gid krs acid cc res rates)

scheduleJSONActions :: [InitialAction] -> Scheduler Action -> IO ()
scheduleJSONActions as scheduler =
  forM_ as $ \(InitialAction aId a s) ->
    scheduleJob scheduler (Just aId) s a

readAnnouncementChannelId :: FilePath -> IO ChannelId
readAnnouncementChannelId file = do
  cidText <- readFile file
  case readMaybe cidText of
    Nothing -> do
      Logging.log Error $
        "Could not parse the announcement channel ID in " <> T.pack _ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE <> "."
      error "fatal error"
    Just acid -> do
      Logging.log Info $
        "Successfully parsed the announcement channel ID in " <> T.pack _ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE <> "."
      return acid
