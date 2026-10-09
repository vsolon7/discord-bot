module Vesbot.Handler.Messages where

import Vesbot.Config (Config)
import Vesbot.Database (DBConnection)
import Vesbot.Utils (liftIO, fromBot)
import Vesbot.Logging as Logging (echo)

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R

messageHandler :: Config -> DBConnection -> Message -> DiscordHandler ()
messageHandler cfg dbconn =
  \mess -> do
    if fromBot mess
      then return ()
      else do
        return ()
