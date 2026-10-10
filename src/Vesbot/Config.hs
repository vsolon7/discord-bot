module Vesbot.Config
  ( InitialEnv
  , Config
  , cfgApiToken
  , cfgGuildId
  , envDBConnection
  , envResponses
  , envNotifications
  , initBot
  ) where

import Vesbot.Responses as Responses (KeywordResponse, initKeywordResponses)
import Vesbot.Database.Types (DBConnection)
import Vesbot.Database as Database (initDBConnection)
import Vesbot.Notifications.Types (NotificationsEnv(..), Waker(..))

import Discord.Types

import Text.Read (readMaybe)
import Control.Concurrent.MVar (newEmptyMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


data Config = Config
  { cfgApiToken :: T.Text
  , cfgGuildId :: GuildId
  }


data InitialEnv = InitialEnv
  { envDBConnection :: DBConnection
  , envResponses :: [KeywordResponse]
  , envNotifications :: NotificationsEnv
  }


_DATABASE_FILE :: FilePath
_DATABASE_FILE = "appdata/database/data.db"
_RESPONSES_FILE :: FilePath
_RESPONSES_FILE = "appdata/json/responses.json"
_HOLIDAYS_FILE :: FilePath
_HOLIDAYS_FILE = "appdata/json/holidays.json"


getToken :: IO T.Text
getToken = TIO.readFile "../discord-bot-apidata/token"


getGuildId :: IO GuildId
getGuildId = do
  gids <- readFile "../discord-bot-apidata/guildid"
  case readMaybe gids of
    Just g -> return g
    Nothing -> error "Could not read guild id from `apidata/guildid`"


initBot :: IO (InitialEnv, Config)
initBot = do
  t <- getToken
  gid <- getGuildId

  krs <- Responses.initKeywordResponses _RESPONSES_FILE
  conn <- Database.initDBConnection _DATABASE_FILE

  waker <- Waker <$> newEmptyMVar :: IO Waker
  let notificationEnv = NotificationsEnv conn waker
 
  return (InitialEnv conn krs notificationEnv, Config t gid)




