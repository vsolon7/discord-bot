{-# LANGUAGE OverloadedStrings #-}

module Vesbot.Utils where

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
import UnliftIO.Concurrent
import Discord
import Discord.Types
import Discord.Interactions
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Scientific (toRealFloat)
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

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x:_) = x

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

-- Helper function that extracts the UserId from a MemberOrUser type
getUserData :: MemberOrUser -> Maybe UserId
getUserData (
  MemberOrUser (
    Left (
      GuildMember
        { memberUser = Just (
            User { userId = uid }
          )
        }
      )
    )
  ) = Just uid

getUserData _ = Nothing

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

extractStringOption :: T.Text -> OptionsData -> Maybe T.Text
extractStringOption _ (OptionsDataSubcommands _) = Nothing
extractStringOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueString n (Right v) <- vs, n == optionName] of
    (v:_) -> Just v
    _     -> Nothing

extractIntegerOption :: T.Text -> OptionsData -> Maybe Integer
extractIntegerOption _ (OptionsDataSubcommands _) = Nothing
extractIntegerOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueInteger n (Right v) <- vs, n == optionName] of
    (v:_) -> Just v
    _     -> Nothing

extractNumberOption :: T.Text -> OptionsData -> Maybe Double
extractNumberOption _ (OptionsDataSubcommands _) = Nothing
extractNumberOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueNumber n (Right v) <- vs, n == optionName] of
    (v:_) -> Just (toRealFloat v)
    _     -> Nothing

extractUserOption :: T.Text -> OptionsData -> Maybe UserId
extractUserOption _ (OptionsDataSubcommands _) = Nothing
extractUserOption optionName (OptionsDataValues vs) =
  case [uid | OptionDataValueUser n uid <- vs, n == optionName] of
    (uid:_) -> Just uid
    _     -> Nothing

makeEphemeral :: InteractionResponseMessage -> InteractionResponse
makeEphemeral mess =
  InteractionResponseChannelMessage
    mess
      { interactionResponseMessageFlags =
          Just (InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral])
      }

ephemeralResponseBasic t = makeEphemeral . interactionResponseMessageBasic $ t

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
