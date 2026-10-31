{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Reactions where

import Vesbot.Utils (liftIO, showT)
import Vesbot.Parsing (emojiToText)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config (Config(..), Env(..))
import Vesbot.Activity (modifyReactionActivity, addEmojiUse)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Control.Monad (when)
import Data.Time (getCurrentTime)

reactionAddHandler :: Config -> Env -> ReactionInfo -> DiscordHandler ()
reactionAddHandler _ env info@(ReactionInfo _ _ _ _ emoji) = do
  now <- liftIO getCurrentTime
  mUsers <- extractReactionInfo info
  case mUsers of
    Nothing -> return ()
    Just (reacter, reactee) ->
      -- self reactions don't count and we don't care to track how many reactions bots get.
      when (userId reacter /= userId reactee && not (userIsBot reactee || userIsBot reacter)) $
          let reacteeId = userId reactee
              reacterId = userId reacter
              eName = emojiToText emoji
          in liftIO $ do
            modifyReactionActivity (envDBConnection env) reacterId reacteeId
            addEmojiUse (envDBConnection env) eName now

reactionRemoveHandler :: Config -> Env -> ReactionInfo -> DiscordHandler ()
reactionRemoveHandler _ _ _ = return () -- maybe this will be used in the future

extractReactionInfo :: ReactionInfo -> DiscordHandler (Maybe (User, User))
extractReactionInfo (ReactionInfo rUid mGid cid oMid _) = do
  rUsrOrErr <- restCall $ R.GetUser rUid -- the user that reacted (or an error)
  oMsgOrErr <- restCall $ R.GetChannelMessage (cid, oMid) -- the original message that was reacted to (or an error)
  case rUsrOrErr of
    Left err -> do
      Logging.log Error $
        "Failed to get the reacting user when trying update reaction related info. \
        \Discord returned the error:\n" <> showT err
      return Nothing
    Right rUsr ->
      case mGid of
        Nothing -> do
          Logging.log Warn
            "Tried to update reaction activity for a reaction that did not happen in a guild. The bot\
            \is not designed for this use case. Ignoring the reaction..."-- if the reaction didn't happen in a guild, we don't care
          return Nothing
        Just _ -> case oMsgOrErr of
          Left err -> do
            Logging.log Error $
              "Failed to get the original message when trying to update a user's emoji reaction score. \
              \Discord returned the error:\n" <> showT err
            return Nothing
          Right oMsg -> do -- the message the user reacted to.
            let op = messageAuthor oMsg
            return $ Just (rUsr, op)

