module Vesbot.Config
  ( InitialEnv
  , Config
  , cfgApiToken
  , cfgGuildId
  , envDBConnection
  , envResponses
  , initBot
  ) where

import Vesbot.Responses as Responses (KeywordResponse, initKeywordResponses)
import Vesbot.Database as Database (DBConnection, initDBConnection)

import Discord.Types

import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


data Config = Config
  { cfgApiToken :: T.Text
  , cfgGuildId :: GuildId
  }
  
data InitialEnv = InitialEnv
  { envDBConnection :: DBConnection
  , envResponses :: [KeywordResponse]
  }

_DATABASE_FILE :: FilePath
_DATABASE_FILE = "appdata/database/data.db"
_RESPONSES_FILE :: FilePath
_RESPONSES_FILE = "appdata/json/responses.json"


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
 
  return (InitialEnv conn krs, Config t gid)




