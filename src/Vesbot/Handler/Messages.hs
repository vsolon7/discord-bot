{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Messages where

import Vesbot.Utils (liftIO, fromBot, sendMessage)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config (Config(..), Env(..))
import Vesbot.Database.Types (DBConnection)
import Vesbot.Activity (updateMessageActivity)
import Vesbot.Responses (KeywordResponseData(..), KeywordResponse(..), LuckyEmoji(..), createKeywordResponse)

import Discord
import Discord.Types

import qualified Data.Text as T
import Data.List (find)
import Data.Time (getCurrentTime)
import System.Random


luckyEmojiReact = createKeywordResponse (KeywordResponseData "lucky" "" "sunturtle:1555089683107676240" "")

messageHandler :: Config -> Env -> Message -> DiscordHandler ()
messageHandler cfg env =
  \msg ->
    if fromBot msg
      then return ()
    else do
      -- | Lucky emoji dice roll
      let le = cfgLuckyEmoji cfg
      r <- liftIO $ randomRIO (1, luckyEmojiChance le) :: DiscordHandler Int
      if r == 1 then luckyEmojiHandler le msg else return ()

      -- | Check if the message contains one of the response keywords as a substring.
      -- Note that we only check for the *first* such substring match.
      case find (\r -> msg `contains` (responseKeyword . responseData $ r)) (cfgResponses cfg) of
        Nothing       -> return ()
        Just response -> responseHandler response msg

      -- | Update message activity
      case messageGuildId msg of
        Nothing ->
          Logging.log Warn $ "Received message does not have an associated Guild ID! Ignoring it... the bot should \
          \only be used in a server."
        Just gid ->
          if gid /= cfgGuildId cfg
            then Logging.log Error $ "Received message with a different Guild ID than the Guild ID the bot was configured \
                              \ with. Does the token correspond to the Guild ID in the guildid file?"
          else liftIO $ updateMessageActivity (envDBConnection env) msg
  where
    contains :: Message -> T.Text -> Bool
    contains msg t = t `T.isInfixOf` (T.toLower . messageContent $ msg)
