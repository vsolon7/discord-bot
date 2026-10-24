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
  ) where

import Vesbot.Utils (void, threadDelay, liftIO, sendMessage, reactToMessage)
import Vesbot.Parsing (parseJSON)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import qualified Data.Text as T
import qualified Data.Aeson as AE

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
  { rareEmojiHandler :: Message -> DiscordHandler ()
  , rareEmojiChance :: Int
  }


createKeywordResponse :: KeywordResponseData -> KeywordResponse
createKeywordResponse res = KeywordResponse
  { responseData = res
  , responseHandler = \msg -> do
      case (responseEmoji res) of
        "" -> return ()
        _  -> do
          reactToMessage msg (responseEmoji res)
          liftIO . threadDelay $ 100000
      case (responseOutput res) of
        ""      -> return ()
        content -> do
          sendMessage (messageChannelId msg) (Just $ messageId msg) content
  }

createRareEmoji :: RareEmojiData -> RareEmoji
createRareEmoji ld = RareEmoji
  { rareEmojiHandler = \msg -> reactToMessage msg (rareEmojiDataId ld)
  , rareEmojiChance = rareEmojiDataChance ld
  }

initKeywordResponses :: FilePath -> IO [KeywordResponse]
initKeywordResponses krFile = do
  krd <- parseJSON krFile :: IO (Maybe [KeywordResponseData])
  case krd of
    Nothing -> return []
    Just res -> return (map createKeywordResponse res)
