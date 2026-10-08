{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Responses where

import qualified Data.Text as T
import qualified Data.Aeson as AE
import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R
import GHC.Generics (Generic)
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

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
        "null" -> return ()
        _      -> do
          void . restCall $
            R.CreateReaction
              (messageChannelId mess, messageId mess)
              (responseEmoji res)
          liftIO . threadDelay $ 100000
      case (responseOutput res) of
        "null" -> return ()
        txt    -> do
          void . restCall $
            R.CreateMessage
              (messageChannelId mess)
              txt
  }
