{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.List (mapAccumL)
import Data.Text (Text)
import UnliftIO (liftIO)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.IO as TIO
import qualified Discord.Requests as R
import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString as AT
import Control.Monad.IO.Class (MonadIO)
import UnliftIO (liftIO)
import UnliftIO.Concurrent
import Discord
import Discord.Types
import Discord.Interactions
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import GHC.Generics

_KEYWORD_RESPONSE_FILEPATH = "appdata/keywords/keywords.json"
_ARGTIMER_FILEPATH = "appdata/savedtime"
_PREDICTIONS_FILEPATH = "appdata/wagers/predictions.json"

--
-- Misc.
--

echo :: MonadIO m => Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> Text
showT = T.pack . show

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

startsWith :: Message -> Text -> Bool
startsWith mess t = t `T.isPrefixOf` (T.toLower . messageContent $ mess)

--
-- API related utilities
--

getToken :: IO T.Text
getToken = TIO.readFile "../discord-bot-apidata/token"

getGuildId :: IO GuildId
getGuildId = do
  gids <- readFile "../discord-bot-apidata/guildid"
  case readMaybe gids of
    Just g -> return g
    Nothing -> error "Could not read guild id from `apidata/guildid`"

-- | Given the test server and an action operating on a channel id, get the
-- first text channel of that server and use the action on that channel.
actionWithChannelId :: GuildId -> (ChannelId -> DiscordHandler a) -> DiscordHandler a
actionWithChannelId serverid f = do
  Right chans <- restCall $ R.GetGuildChannels serverid
  (f . channelId) (head $ filter isTextChannel chans)
  where
    isTextChannel :: Channel -> Bool
    isTextChannel ChannelText {} = True
    isTextChannel _ = False

--
-- JSON Parsing
--

parseJSON :: FromJSON a => FilePath -> IO (Maybe a)
parseJSON path = do
  jsonData <- BS.readFile path
  let decoded = A.decodeStrict jsonData
  case decoded of
    Nothing -> do
      echo $ "Error parsing the JSON Data in " <> T.pack path <> "."
      return Nothing
    Just d  -> return (Just d)
