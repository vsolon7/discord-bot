{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Notifications
  ( saveNotification
  , startNotifier
  ) where

import Vesbot.Logging (echo)
import Vesbot.Utils (void, liftIO, forkIO, forM_, showT, threadDelay)
import Vesbot.Database.Types (DBConnection)
import Vesbot.Notifications.Types (Notification(..), NotifierControl, sleep)
import Vesbot.Notifications.Database (saveNotification, getDueNotifications, getNextNotificationTime)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Control.Monad.Reader (runReaderT, ask)
import Control.Monad (forever)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Timeout (timeout)


notificationToMessageRequest :: Notification -> R.ChannelRequest Message
notificationToMessageRequest (Notification cid content reply _ _) =
  let ref = MessageReference reply (Just cid) Nothing False
  in  R.CreateMessageDetailed cid $
        R.MessageDetailedOpts
          { R.messageDetailedContent = content
          , R.messageDetailedTTS = False
          , R.messageDetailedEmbeds = Nothing
          , R.messageDetailedAttachments = Nothing
          , R.messageDetailedUpload = []
          , R.messageDetailedAllowedMentions = Nothing
          , R.messageDetailedReference = Just ref
          , R.messageDetailedComponents = Nothing
          , R.messageDetailedStickerIds = Nothing
          }


startNotifier :: DBConnection -> NotifierControl -> DiscordHandler ()
startNotifier dbconn controller = do
  h <- ask
  void . liftIO . forkIO . forever $ do
    now <- getCurrentTime
    ns <- getDueNotifications dbconn now
    forM_ ns $ \n -> do
      res <- runReaderT (restCall $ notificationToMessageRequest n) h
      case res of
        Left err -> echo $
          "Failed to send notification to the server. \
          \Notification data was:\n" <> showT n <> "\n\n" <>
          "The discord API call returned the error:\n" <> showT err
        Right _ -> return ()
      threadDelay (3 * 1000000) -- wait 3 seconds between notifications
    
    next <- getNextNotificationTime dbconn
    case next of
      Nothing -> sleep controller
      Just t -> do
        let until = min 3600 (diffUTCTime t now) -- rest for at most 1 hour before checking for a new notification
            sleepFor = max 0 (ceiling $ until * 1000000) :: Int
        void (timeout sleepFor $ sleep controller)
