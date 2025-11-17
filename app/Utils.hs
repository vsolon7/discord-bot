module Utils where

import Data.Text (Text)
import UnliftIO (liftIO)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)

--
-- Misc.
--

echo :: MonadIO m => Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> Text
showT = T.pack . show


--
-- API related utilities
--

getToken :: IO T.Text
getToken = TIO.readFile "apidata/token"

getGuildId :: IO GuildId
getGuildId = do
  gids <- readFile "apidata/guildid"
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
                   ++ show secT ++ " " ++ plural secT "second"
  where
    secT = splitTime !! 0
    minT = splitTime !! 1
    hourT = splitTime !! 2
    dayT = splitTime !! 3
    splitTime = splitSecs intTime
    (intTime, _) = properFraction time

    splitSecs n = go divMod n [86400, 3600, 60, 1] []
      where
        go f x [] rs = rs
        go f x (y:ys) rs = go f (snd . f x $ y) ys ((fst . f x $ y):rs)

    plural :: Int -> String -> String
    plural n str = if (n /= 1) then (str ++ "s") else str

