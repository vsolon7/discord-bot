{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Responses
  ( KeywordResponse(..)
  , KeywordResponseData(..)
  , RareEmojiData(..)
  , RareEmoji(..)
  , initKeywordResponses
  , createKeywordResponse
  , createRareEmoji
  , selectNRandomRareEmojis
  ) where

import Vesbot.Logging as Logging (LogType(..), log)
import Vesbot.Utils (threadDelay, liftIO, sendMessage, reactToMessage)
import Vesbot.Parsing (parseJSON)

import Discord
import Discord.Types

import qualified Data.Text as T
import qualified Data.Aeson as AE
import System.Random.Stateful (uniformRM, globalStdGen)
import GHC.Generics (Generic)


data KeywordResponseData = KeywordResponseData
  { responseName :: T.Text
  , responseKeyword :: T.Text
  , responseEmoji :: T.Text
  , responseOutput :: T.Text
  } deriving (Generic, Show, AE.FromJSON)

data KeywordResponse = KeywordResponse
  { responseData :: KeywordResponseData
  , responseHandler :: Message -> DiscordHandler ()
  }

data RareEmojiData = RareEmojiData
  { rareEmojiDataId :: T.Text
  , rareEmojiDataChance :: Int
  } deriving (Generic, Show, AE.FromJSON)

data RareEmoji = RareEmoji
  { rareEmojiId :: T.Text
  , rareEmojiHandler :: Message -> DiscordHandler ()
  , rareEmojiChance :: Int
  }


createKeywordResponse :: KeywordResponseData -> KeywordResponse
createKeywordResponse res = KeywordResponse
  { responseData = res
  , responseHandler = \msg -> do
      case responseEmoji res of
        "" -> return ()
        _  -> do
          reactToMessage msg (responseEmoji res)
          liftIO . threadDelay $ 100000
      case responseOutput res of
        ""      -> return ()
        content -> do
          sendMessage (messageChannelId msg) (Just $ messageId msg) content
  }

createRareEmoji :: RareEmojiData -> RareEmoji
createRareEmoji ld = RareEmoji
  { rareEmojiId = rareEmojiDataId ld
  , rareEmojiHandler = \msg -> reactToMessage msg (rareEmojiDataId ld)
  , rareEmojiChance = rareEmojiDataChance ld
  }

initKeywordResponses :: FilePath -> IO [KeywordResponse]
initKeywordResponses krFile = do
  krd <- parseJSON krFile :: IO (Maybe [KeywordResponseData])
  case krd of
    Nothing -> return []
    Just res -> return (map createKeywordResponse res)


selectNRandomRareEmojis :: Int -> [RareEmoji] -> IO [RareEmoji]
selectNRandomRareEmojis n res = do
  let totalWeight = sum . map rareEmojiChance $ res
      numRes = length res
      numReacts = min numRes n -- can't select more emojis than we have
  go totalWeight numReacts [] []
  where
    ixres = zip [1..] res
    go :: Int -> Int -> [Int] -> [(Int, RareEmoji)] -> IO [RareEmoji]
    go _ 0 _ selected = return (map snd selected)
    go w reactsLeft ignore selected = do
      r <- selectReact w ignore ixres
      let w' = sum . map rareEmojiChance $ [ s | (m,s) <- ixres, m `notElem` ignore ]
      go w' (reactsLeft-1) (fst r:ignore) (r:selected)

selectReact :: Int -> [Int] -> [(Int, RareEmoji)] -> IO (Int, RareEmoji)
selectReact totalWeight ignore res = do
  roll <- uniformRM (1, totalWeight) globalStdGen :: IO Int
  go roll res
  where
    go _ [] = do
      Logging.log Error "Tried to select an emoji from an empty list of emojis."
      error "fatal error"
    go _ [r] = return r
    go num ((n,r):rs) =
      if n `elem` ignore
        then go num rs
        else do
          let num' = num - rareEmojiChance r
          if num' <= 0
            then return (n,r)
            else go num' rs
