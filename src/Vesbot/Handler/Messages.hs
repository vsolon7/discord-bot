{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Messages where

import Vesbot.Utils (liftIO, fromBot, sendMessage, traverse_, forM, forM_, threadDelay, announce, showT)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config (Config(..), Env(..), DropTableRates(..))
import Vesbot.Database.Types (DBConnection)
import Vesbot.Activity (updateMessageActivity)
import Vesbot.Responses (KeywordResponseData(..), KeywordResponse(..), RareEmoji(..), createKeywordResponse)

import Discord
import Discord.Types

import qualified Data.Text as T
import Control.Monad (unless)
import Data.List (find)
import Data.Time (getCurrentTime)
import System.Random


messageHandler :: Config -> Env -> Message -> DiscordHandler ()
messageHandler cfg env =
  \msg ->
    if fromBot msg
      then return ()
    else do
      -- | Lucky emoji dice rolls
      rareEmojiRoll cfg msg

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


rareEmojiRoll :: Config -> Message -> DiscordHandler ()
rareEmojiRoll cfg msg = do
  let res = cfgRareEmoji cfg
      rates = cfgDropTableRates cfg
  selected <- liftIO $ selectRandomEmojiReacts rates res
  forM_ selected $ \s -> do
    rareEmojiHandler s msg
    threadDelay 500000 -- wait 0.5 seconds in between reacts
  unless (null selected) $ do
    let cid = showT . messageChannelId $ msg
        mid = showT . messageId $ msg
        link = "https://discord.com/channels/" <> (showT . cfgGuildId $ cfg) <> "/" <> cid <> "/" <> mid
        message = "HOLY. CHECK IT: " <> link
    announce (cfgAnnouncementCID cfg) message

-- | TODO: unfinished
computeNumReacts :: DropTableRates -> IO Int
computeNumReacts (DropTableRates rates) = go rates 0
  where
    go rates n = do
      let chance = rates n
      roll <- randomRIO (1, chance) :: IO Int
      if roll == chance
        then go rates (n+1)
        else return n

selectReact :: Int -> [Int] -> [(Int, RareEmoji)] -> IO (Int, RareEmoji)
selectReact totalWeight ignore res = do
  roll <- randomRIO (1, totalWeight) :: IO Int
  go roll res
  where
    go _ [] = do
      Logging.log Error $ "Tried to select an emoji from an empty list of emojis after doing a rare drop roll."
      error "fatal error"
    go _ [r] = return r
    go num ((n,r):rs) =
      if n `elem` ignore
        then go num rs
        else do
          let num' = num - (rareEmojiChance r)
          if num' <= 0
            then return (n,r)
            else go num' rs

selectRandomEmojiReacts :: DropTableRates -> [RareEmoji] -> IO [RareEmoji]
selectRandomEmojiReacts dtr res = do
  let numEmojis = length res
      totalWeight = sum . map rareEmojiChance $ res
  numReacts <- computeNumReacts dtr
  go totalWeight numReacts [] []
  where
    ixres = zip [1..] res
    go :: Int -> Int -> [Int] -> [(Int, RareEmoji)] -> IO [RareEmoji]
    go _ 0 _ selected = return (map snd selected)
    go w n ignore selected = do
      r <- selectReact w ignore ixres
      let w' = sum . map rareEmojiChance $ [ r | (n,r) <- ixres, n `notElem` ignore ]
      go w' (n-1) (fst r:ignore) (r:selected)
