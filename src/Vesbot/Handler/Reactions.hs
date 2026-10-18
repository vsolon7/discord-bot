{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Reactions where

import Vesbot.Utils (liftIO, showT, fromBot)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config (Config(..), Env(..))
import Vesbot.Database.Types (DBConnection)
import Vesbot.Activity (modifyReactionActivity, addEmojiUse)
import Vesbot.Currency (updateCurrency, CurrencyUpdate(..))

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Control.Monad (unless)
import Data.Time (getCurrentTime)

reactionHandler :: Config -> Env -> ReactionInfo -> DiscordHandler ()
reactionHandler cfg env (ReactionInfo rUid mGid cid oMid emoji) = do
  now <- liftIO getCurrentTime
  ruOrErr <- restCall $ R.GetUser rUid -- the user that reacted (or an error)
  oMsgOrErr <- restCall $ R.GetChannelMessage (cid, oMid) -- the original message that was reacted to (or an error)
  case ruOrErr of
    Left err -> Logging.log Error $
      "Failed to get the reacting user when trying update a user's emoji reaction score. \
      \Discord returned the error:\n" <> showT err
    Right rUsr ->
      unless (userIsBot rUsr) $ case mGid of -- we don't care if a bot reacts to a message 
        Nothing -> Logging.log Warn "Tried to update reaction activity for a reaction that did not happen in a guild. The bot\
          \is not designed for this use case. Ignoring the reaction..."-- if the reaction didn't happen in a guild, we don't care
        Just gid -> case oMsgOrErr of
          Left err ->
            Logging.log Error $
              "Failed to get the original message when trying to update a user's emoji reaction score. \
              \Discord returned the error:\n" <> showT err
          Right oMsg -> do -- the message the user reacted to.
            let op = messageAuthor oMsg
                opid = userId op
            -- self reactions don't count and we don't care to track how many reactions bots get.
            unless (userId op == rUid || userIsBot op) $
              liftIO $ do
                modifyReactionActivity (envDBConnection env) rUid opid
                updateCurrency (envDBConnection env) (CurrencyUpdate opid 0.1)
                addEmojiUse (envDBConnection env) (emojiName emoji) now
  

