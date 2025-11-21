{-# LANGUAGE OverloadedStrings #-}

module Utils where

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
import Control.Monad (void)
import Discord
import Discord.Types
import Discord.Interactions
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)

type ParsedJSONKeywordResponse = (Text,(Text,Text,Text))

data SlashCommand = SlashCommand
  { commandName :: Text
  , commandRegistration :: Maybe CreateApplicationCommand
  , commandHandler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

data KeywordResponse = KeywordResponse
  { responseName :: Text
  , responseKeyword :: Text
  , responseHandler :: Message -> DiscordHandler ()
  }

_KEYWORD_RESPONSE_FILE_PATH = "appdata/keywords/keywords.json"
_ARGTIMER_FILEPATH = "appdata/savedtime"

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
    Just g -> pure g
    Nothing -> error "could not read guild id from `apidata/guildid`"

-- | Given the test server and an action operating on a channel id, get the
-- first text channel of that server and use the action on that channel.
actionWithChannelId :: GuildId -> (ChannelId -> DiscordHandler a) -> DiscordHandler a
actionWithChannelId testserverid f = do
  Right chans <- restCall $ R.GetGuildChannels testserverid
  (f . channelId) (head (filter isTextChannel chans))
  where
    isTextChannel :: Channel -> Bool
    isTextChannel ChannelText {} = True
    isTextChannel _ = False

--
-- File Reading Utilities
--

getSavedTime :: FilePath -> IO UTCTime
getSavedTime path = do
  time <- TIO.readFile path
  return . read . T.unpack $ time

getTimeDiff :: FilePath -> IO NominalDiffTime
getTimeDiff path = do
  prevTime <- TIO.readFile path
  let prevTime1 = read . T.unpack $ prevTime
  currTime <- getCurrentTime
  return $ diffUTCTime currTime prevTime1

setSavedTime :: FilePath -> UTCTime -> IO ()
setSavedTime path time = do
  TIO.writeFile path (showT time)

formatDiffTime :: NominalDiffTime -> String
formatDiffTime time = show dayT ++ " " ++ plural dayT "day" ++ ", "
                   ++ show hourT ++ " " ++ plural hourT "hour" ++ ", "
                   ++ show minT ++ " " ++ plural minT "minute" ++ ", "
                   ++ show secT ++ " " ++ plural secT "second" ++ "."
  where
    secT = splitTime !! 0
    minT = splitTime !! 1
    hourT = splitTime !! 2
    dayT = splitTime !! 3
    splitTime = splitSecs intTime
    (intTime, _) = properFraction time

    --86400 seconds in an hour, 3600 seconds in a minute, etc.
    splitSecs n = go divMod n [86400, 3600, 60, 1] []
      where
        --given a function f :: a -> a' -> (b,a), a starting input x :: a, and a list of secondary inputs ys :: [a'],
        --repeatedly feed in the second output of f into f _ y again and keeps a list of the first outputs
        --obtained from this process.
        go f x [] rs = rs
        go f x (y:ys) rs = go f (snd $ f x y) ys ((fst $ f x y):rs)

    plural :: Int -> String -> String
    plural n str = if (n /= 1) then (str ++ "s") else str

--
-- JSON Parsing
--

responseFromJSONTemplate :: ParsedJSONKeywordResponse -> KeywordResponse
responseFromJSONTemplate (name,(key,emoji,res)) = KeywordResponse
  { responseName = name
  , responseKeyword = key
  , responseHandler = \mess -> do
      case emoji of
        "null" -> pure ()
        _      -> do
          void . restCall $
            R.CreateReaction
              (messageChannelId mess, messageId mess)
               emoji
          threadDelay (10^(5 :: Int))

      void . restCall $
        R.CreateMessage
          (messageChannelId mess)
          res
  }

parseJSONResponses :: FilePath -> IO [KeywordResponse]
parseJSONResponses path = do
  jsonData <- BS.readFile path
  let decoded = A.decodeStrict jsonData :: Maybe [(Text, (Text,Text,Text))]
  case decoded of
    Nothing -> do
      print $ "Error parsing the JSON Data in " ++ path ++ "."
      return []
    Just d  -> do
      return $ map responseFromJSONTemplate d
