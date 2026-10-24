{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, MultiWayIf #-}
module Vesbot.Config
  ( Env(..)
  , Config(..)
  , CurrencyConfig(..)
  , DropTableRates(..)
  , initBot
  ) where

import Vesbot.Utils (forM_, traverse_)
import Vesbot.Parsing (parseJSON)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Actions (Action(..))
import Vesbot.Responses as Responses (KeywordResponse, RareEmoji, RareEmojiData(..), createRareEmoji, initKeywordResponses)
import Vesbot.Database.Types (DBConnection)
import Vesbot.Database as Database (initDBConnection)
import JSONScheduler.Scheduler.Bell (Bell(..))
import JSONScheduler.Scheduler as Scheduler (Scheduler(..), Schedule(..), initScheduler, scheduleJob)

import Discord.Types

import Data.Time
import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, encode)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, catMaybes)
import Control.Concurrent.MVar (newEmptyMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BS (putStr)

data CurrencyConfig = CurrencyConfig
  { currencyName :: T.Text
  , currencySymbolKilo :: T.Text
  , currencySymbolNormal :: T.Text
  , currencySymbolMilli :: T.Text
  } deriving (Show, Generic, ToJSON, FromJSON)

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
_SCHEDULER_DATABASE_FILE  = "appdata/scheduler/scheduler.db"
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
  | n > 0 = 2
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
      error "Fatal error."


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
            Logging.log Error $ "Lucky emoji chance sent to <0."
        | rareEmojiDataChance red == 0 = do
            Logging.log Warn $ "Lucky emoji chance set to 0. This means that this reaction is removed from the drop table."
        | otherwise = return ()

  traverse_ validateRareEmoji reds
  let res = map createRareEmoji reds

  let rates = DropTableRates _DROP_TABLE_RATES

  return (t, Env conn s, Config gid krs acid cc res rates)


scheduleJSONActions :: [InitialAction] -> Scheduler Action -> IO ()
scheduleJSONActions as scheduler =
  forM_ as $ \(InitialAction id a s) ->
    scheduleJob scheduler (Just id) s a

readAnnouncementChannelId :: FilePath -> IO ChannelId
readAnnouncementChannelId file = do
  cidText <- readFile _ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE
  let acid = read cidText
  Logging.log Info $ "Successfully parsed announcement channel ID in " <> (T.pack _ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE) <> "."
  return acid
