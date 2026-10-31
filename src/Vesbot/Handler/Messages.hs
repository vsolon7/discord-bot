{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Messages where

import Vesbot.Utils (liftIO, fromBot, forM_, threadDelay, announce, showT)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config (Config(..), Env(..), DropTableRates(..))
import Vesbot.Activity (updateMessageActivity)
import Vesbot.Responses (KeywordResponseData(..), KeywordResponse(..), RareEmoji(..), selectNRandomRareEmojis)
import Vesbot.Currency (EmojiUpdate(..), updateEmojis)

import Discord
import Discord.Types

import qualified Data.Text as T
import Control.Monad (unless, when)
import Data.List (find)
import System.Random.Stateful (uniformRM, globalStdGen)


messageHandler :: Config -> Env -> Message -> DiscordHandler ()
messageHandler cfg env =
  \msg ->
    if fromBot msg
      then return ()
    else do
      -- | Lucky emoji dice rolls
      rareEmojiRoll cfg env msg

      -- | Check if the message contains one of the response keywords as a substring.
      -- Note that we only check for the *first* such substring match.
      case find (\r -> msg `contains` (responseKeyword . responseData $ r)) (cfgResponses cfg) of
        Nothing       -> return ()
        Just response -> responseHandler response msg

      -- | Update message activity
      case messageGuildId msg of
        Nothing ->
          Logging.log Warn
            "Received message does not have an associated Guild ID! Ignoring it... the bot should \
            \only be used in a server."
        Just gid ->
          if gid /= cfgGuildId cfg
            then Logging.log Error
              "Received message with a different Guild ID than the Guild ID the bot was configured with.\
              \Does the token correspond to the Guild ID in the guildid file?"
            else liftIO $
              updateMessageActivity (envDBConnection env) msg
  where
    contains :: Message -> T.Text -> Bool
    contains msg t = t `T.isInfixOf` (T.toLower . messageContent $ msg)


rareEmojiRoll :: Config -> Env -> Message -> DiscordHandler ()
rareEmojiRoll cfg env msg = do
  let res = cfgRareEmoji cfg
      rates = cfgDropTableRates cfg
  selected <- liftIO $ selectRandomEmojiReacts rates res
  forM_ selected $ \e -> do
    rareEmojiHandler e msg
    threadDelay 500000 -- wait 0.5 seconds in between reacts
  unless (null selected) $ do
    let uid = userId . messageAuthor $ msg
        ping = "<@" <> showT uid <> ">"
        eUpdate = EmojiUpdate uid [(rareEmojiId re, 1) | re <- selected ]
        cid = showT . messageChannelId $ msg
        mid = showT . messageId $ msg
        link = "https://discord.com/channels/" <> (showT . cfgGuildId $ cfg) <> "/" <> cid <> "/" <> mid
        message = "HOLY. " <> ping <> " just got a random drop of " <> showT (length selected) <> " rare emoji react(s). CHECK IT: " <> link
    liftIO $ updateEmojis (envDBConnection env) eUpdate
    announce (cfgAnnouncementCID cfg) message

computeNumReacts :: DropTableRates -> IO Int
computeNumReacts (DropTableRates rates) = go rates 0
  where
    go r n = do
      let chance = rates n
      roll <- uniformRM (1, chance) globalStdGen :: IO Int
      if roll == chance
        then go r (n+1)
        else do
          when (n > 0) $ Logging.log Info $ "Reacting " <> showT n <> " times with rare emoji(s) to a message."
          return n

selectRandomEmojiReacts :: DropTableRates -> [RareEmoji] -> IO [RareEmoji]
selectRandomEmojiReacts dtr res = do
  n <- computeNumReacts dtr
  selectNRandomRareEmojis n res
