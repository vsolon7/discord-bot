{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module Vesbot.Config
  ( Env(..)
  , Config(..)
  , initBot
  ) where

import Vesbot.Utils (forM_)
import Vesbot.Actions (Action(..))
import Vesbot.Responses as Responses (KeywordResponse, initKeywordResponses)
import Vesbot.Database.Types (DBConnection)
import Vesbot.Database as Database (initDBConnection)
import JSONScheduler.Scheduler.Bell (Bell(..))
import JSONScheduler.Scheduler as Scheduler (Scheduler(..), Schedule(..), initScheduler, scheduleJob)

import Discord.Types

import Data.Time
import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, encode)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Concurrent.MVar (newEmptyMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BS (putStr)


data Config = Config
  { cfgGuildId :: GuildId
  , cfgResponses :: [KeywordResponse]
  , cfgAnnouncementCID :: ChannelId
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


getToken :: IO T.Text
getToken = TIO.readFile "../discord-bot-apidata/token"


getGuildId :: IO GuildId
getGuildId = do
  gids <- readFile "../discord-bot-apidata/guildid"
  case readMaybe gids of
    Just g -> return g
    Nothing -> error "Could not read guild id from `apidata/guildid`"


initBot :: IO (T.Text, Env, Config)
initBot = do
  t <- getToken
  gid <- getGuildId

  s <- Scheduler.initScheduler _SCHEDULER_DATABASE_FILE :: IO (Scheduler Action)
  krs <- Responses.initKeywordResponses _RESPONSES_FILE
  conn <- Database.initDBConnection _DATABASE_FILE

  hs <- readInitialActions _INITIAL_ACTIONS_FILE
  scheduleInitialActions hs s

  acid <- readAnnouncementChannelId _ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE

  return (t, Env conn s, Config gid krs acid)

readInitialActions :: FilePath -> IO [InitialAction]
readInitialActions file = do
  mas <- eitherDecodeFileStrict file
  case mas of
    Left err -> do
      putStrLn $ "Error parsing initial actions in " ++ _INITIAL_ACTIONS_FILE ++ ". Error was:\n" ++ err
      return []
    Right as -> do
      putStrLn $ "Successfully parsed actions in " ++ _INITIAL_ACTIONS_FILE ++ "."
      return as

scheduleInitialActions :: [InitialAction] -> Scheduler Action -> IO ()
scheduleInitialActions as scheduler =
  forM_ as $ \(InitialAction id a s) ->
    scheduleJob scheduler (Just id) s a

readAnnouncementChannelId :: FilePath -> IO ChannelId
readAnnouncementChannelId file = do
  cidText <- readFile _ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE
  let acid = read cidText
  putStrLn $ "Successfully parsed announcement channel ID in " ++ _ANNOUNCEMENT_CHANNEL_ID_CONFIG_FILE ++ "."
  return acid
