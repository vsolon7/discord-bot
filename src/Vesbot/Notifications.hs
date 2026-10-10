{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Notifications
  ( startNotifier
  ) where

import Vesbot.Logging (echo)
import Vesbot.Utils (void, liftIO, forkIO, forM_, showT, threadDelay)
import Vesbot.Parsing (parseJSON)
import Vesbot.Notifications.Types (NotificationsEnv(..), NotificationDataInternal(..), await)
import Vesbot.Notifications.Database (getDueNotifications, getNextNotificationTime)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Control.Monad.Reader (runReaderT, ask)
import Control.Monad (forever)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Timeout (timeout)
import Data.Maybe (fromMaybe)


notificationToMessageRequest :: NotificationDataInternal -> R.ChannelRequest Message
notificationToMessageRequest (NotificationDataInternal _ cid content reply _ _) =
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


startNotifier :: NotificationsEnv -> DiscordHandler ()
startNotifier (NotificationsEnv dbconn waker) = do
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
    let until = case next of -- wait for at most 1 hour before checking for a new notification
          Nothing -> 3600 --units are seconds
          Just t -> min 3600 (diffUTCTime t now)
        waitFor = max 0 (ceiling $ until * 1000000) :: Int
    void (timeout waitFor $ await waker)
