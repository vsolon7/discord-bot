{-# LANGUAGE OverloadedStrings #-}
module Vesbot.ArgumentTimer.Commands where

import Vesbot.Utils (showT, liftIO, void)
import Vesbot.Database.Types (DBConnection)
import Vesbot.ArgumentTimer.Database (getLastArgumentTime, updateArgumentTime)
import Vesbot.SlashCommands.Types

import Discord
import Discord.Interactions
import qualified Discord.Requests as R

import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.List (intersperse, mapAccumL)
import qualified Data.Text as T


viewAC :: DBConnection -> SlashCommand
viewAC dbconn = SlashCommand
  { commandName = "viewac"
  , commandRegistration = createChatInput "viewac" "View the time since the last obnoxious argument"
  , commandHandler = \intr _ -> do
      mt <- liftIO . getLastArgumentTime $ dbconn
      now <- liftIO getCurrentTime

      let reply = case mt of
            Nothing -> "There have been no obnoxious arguments so far!"
            Just t ->
              let since = now `diffUTCTime` t
              in  "Time since the last obnoxious argument: " <> formatDiffTime since <> "."

      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic reply)
  }
  where
    formatDiffTime :: NominalDiffTime -> T.Text
    formatDiffTime time = T.concat . intersperse ", " $ [ showT n <> " " <> u | (n,u) <- reduceAndPlural ]
      where
      (intTime, _) = properFraction time
      (days, [seconds, minutes, hours]) = mapAccumL divMod intTime [60, 60, 24]
      ps = zip [days, hours, minutes, seconds] ["day", "hour", "minute", "second"]
      reduceAndPlural = map (\(n, u) -> (n, plural n u)) . filter (\p -> fst p /= 0) $ ps

      plural :: Int -> T.Text -> T.Text
      plural n t = if n /= 1 then t <> "s" else t


resetAC :: DBConnection -> SlashCommand
resetAC dbconn = SlashCommand
  { commandName = "resetac"
  , commandRegistration = createChatInput "resetac" "Reset the time since the last obnoxious argument"
  , commandHandler = \intr _ -> do
      now <- liftIO getCurrentTime
      liftIO $ updateArgumentTime dbconn now
      let reply = "Time since the last obnoxious argument: 0 days..."
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic reply)
  }
