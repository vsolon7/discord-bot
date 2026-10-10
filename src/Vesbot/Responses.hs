{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Responses
  ( KeywordResponse
  , KeywordResponseData
  , initKeywordResponses
  , responseName
  , responseKeyword
  , responseEmoji
  , responseOutput
  , responseData
  , responseHandler
  ) where

import Vesbot.Utils (void, threadDelay, liftIO)
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
  } deriving (Generic, Show)

instance AE.FromJSON KeywordResponseData

data KeywordResponse = KeywordResponse
  { responseData :: KeywordResponseData
  , responseHandler :: Message -> DiscordHandler ()
  }


createKeywordResponse :: KeywordResponseData -> KeywordResponse
createKeywordResponse res = KeywordResponse
  { responseData = res
  , responseHandler = \mess -> do
      case (responseEmoji res) of
        "" -> return ()
        _  -> do
          void . restCall $
            R.CreateReaction
              (messageChannelId mess, messageId mess)
              (responseEmoji res)
          liftIO . threadDelay $ 100000
      case (responseOutput res) of
        ""      -> return ()
        content -> do
          let replyMessRef =
                MessageReference (Just (messageId mess)) (Just (messageChannelId mess)) (messageGuildId mess) False
              replyMessOpts =
                R.MessageDetailedOpts content False Nothing Nothing [] Nothing (Just replyMessRef) Nothing Nothing
          void . restCall $
            R.CreateMessageDetailed
              (messageChannelId mess)
              replyMessOpts
  }


initKeywordResponses :: FilePath -> IO [KeywordResponse]
initKeywordResponses krFile = do
  krd <- parseJSON krFile :: IO (Maybe [KeywordResponseData])
  case krd of
    Nothing -> return []
    Just res -> return (map createKeywordResponse res)
