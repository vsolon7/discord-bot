{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Utils
  ( liftIO
  , forkIO
  , threadDelay
  , void
  , forM
  , forM_
  , fromBot
  , showT
  , find
  , ephemeralResponseBasic
  , maybeToEither
  , announce
  , sendMessage
  , ackButtonEditAndRemove
  ) where

import Vesbot.Logging as Logging (log, LogType(..))

import Discord
import Discord.Types
import Discord.Interactions
import Discord.Internal.Rest.Interactions
import qualified Discord.Requests as R
import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import UnliftIO.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forM, forM_)
import Data.List (find)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS


showT :: Show a => a -> T.Text
showT = T.show


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x


-- | make a message ephemeral
makeEphemeral :: InteractionResponseMessage -> InteractionResponse
makeEphemeral mess =
  InteractionResponseChannelMessage
    mess
      { interactionResponseMessageFlags =
          Just (InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral])
      }

ephemeralResponseBasic t = makeEphemeral . interactionResponseMessageBasic $ t


-- | Basic response type functions.

announce :: ChannelId -> T.Text -> DiscordHandler ()
announce acid content = do
  let a = R.CreateMessage acid content in do
    r <- restCall $ R.CreateMessage acid content
    case r of
      Left err -> Logging.log Error $ "Error sending an announcement. Discord returned the error:\n" <> showT err
      _ -> Logging.log Info $ "Successfully sent an announcement."

sendMessage :: ChannelId -> Maybe MessageId -> T.Text -> DiscordHandler ()
sendMessage cid replyTo content =
  let ref = case replyTo of
        Nothing  -> Nothing
        Just mid -> Just (MessageReference (Just mid) (Just cid) Nothing False)
      opts = R.MessageDetailedOpts content False Nothing Nothing [] Nothing ref Nothing Nothing
  in do
    r <- restCall $ R.CreateMessageDetailed cid opts
    case r of
      Left err -> Logging.log Error $ "Error sending a message. Discord returned the error:\n" <> showT err
      _ -> Logging.log Info $ "Successfully sent a message to channel " <> showT cid <> "."

ackButtonEditAndRemove :: InteractionId -> InteractionToken -> Message -> T.Text -> DiscordHandler ()
ackButtonEditAndRemove iid itok omsg addition = do
  let ocontent = messageContent omsg
      reply = ocontent <> "\n" <> addition
      response = InteractionResponseMessage (Just False) (Just reply) Nothing Nothing Nothing (Just []) Nothing []
  result <- restCall $ R.CreateInteractionResponse iid itok (InteractionResponseUpdateMessage response)
  case result of
    Left err -> Logging.log Error $ "Error editing the message a button is attached to. Discord Returned the error:\n" <> showT err
    _        -> return ()
