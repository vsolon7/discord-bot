{-# LANGUAGE OverloadedStrings #-}
module Commands where

import Discord
import Discord.Interactions
import Discord.Internal.Types.Components
import Discord.Types (messageChannelId, messageId, Message)
import UnliftIO (liftIO)
import UnliftIO.Concurrent
import Data.Text (Text)
import Control.Monad (void)
import qualified Discord.Requests as R
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Utils

-- A basic slash command with no options, it just replies with some text, possibly obtained from IO.
basicSlashCommand :: Text    -> -- Slash Command Name
                     Text    -> -- Registration Description
                     IO Text -> -- Text diplayed in the interaction response, possibly obtained with IO
                     SlashCommand -- See Utils.hs

basicSlashCommand name regDesc statefulText
  = SlashCommand
    { commandName = name
    , commandRegistration = createChatInput name regDesc
    , commandHandler = \intr _options -> do
        iomessage <- liftIO $ statefulText
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (interactionResponseBasic iomessage)
    }

-- List of slash commands to register
mySlashCommands :: [SlashCommand]
mySlashCommands = [ping, getCurrTime, resetArgCounter, viewArgCounter, printGiantGlorp, test]

ping :: SlashCommand
ping = basicSlashCommand
         ("ping")
         ("Responds 'pong'")
         (return "pong!")


getCurrTime :: SlashCommand
getCurrTime = basicSlashCommand
                ("currtime")
                ("Displays the Current time in UTC.")
                (do x <- getCurrentTime; return $ "The current time is " <> showT x <> ".")


resetArgCounter :: SlashCommand
resetArgCounter = basicSlashCommand
                    ("resetac")
                    ("Resets the time since the last autistic argument.")
                    (saveTimeIO)
  where
    saveTimeIO :: IO Text
    saveTimeIO = do
      currTime <- getCurrentTime
      setSavedTime _ARGTIMER_FILEPATH currTime
      return "Time since the last autistic argument: 0 days"


viewArgCounter :: SlashCommand
viewArgCounter = basicSlashCommand
                   ("ac")
                   ("Gives the time since the last autistic argument.")
                   (timeDiffIO)
  where
    timeDiffIO = do
      timeDiff <- getTimeDiff _ARGTIMER_FILEPATH
      return $ "Time since the last autistic argument: " <> (T.pack $ formatDiffTime timeDiff) <> "."


printGiantGlorp :: SlashCommand
printGiantGlorp = basicSlashCommand
                    ("giantglorp")
                    ("Prints a giant glorp.")
                    (return . T.pack $ giantGlorp)

test :: SlashCommand
test = SlashCommand
       { commandName = "test"
       , commandRegistration = createChatInput "test" "big button test"
       , commandHandler = \intr _options -> do
           case (interactionChannelId intr) of
             Just icid -> do
               res <- restCall $
                 R.CreateMessageDetailed
                   icid
                   opts
               liftIO . print $ res
             _         -> do
               liftIO . print $ "error getting interactionChannelId for interaction " ++ (show $ interactionId intr)
       }
  where
    opts = R.MessageDetailedOpts "test1" False Nothing Nothing Nothing Nothing (Just [actionRow]) Nothing
    actionRow = ActionRowButtons [b1,b2,b3]
    b1 = Button "b1" False ButtonStylePrimary (Just "BUTTON1") Nothing
    b2 = Button "b2" True ButtonStyleDanger (Just "BUTTON2") Nothing
    b3 = Button "b3" False ButtonStyleSecondary (Just "BUTTON3") Nothing

giantGlorp :: String
giantGlorp = "I⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⢠⣄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⠄⠘⢻⠄⠄⠄⠄⠄⠄⠄⢡⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⠄⠄⠘⡀⠄⠄⠄⠄⠄⠄⠘⡆⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⠄⠄⠄⣇⠄⠄⣀⣀⣀⣤⣄⣷⣤⡀⠄⠄⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⠄⣀⣸⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣠⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⢀⣸⣿⣿⣿⣿⣿⢹⣿⣿⣿⣿⣿⣿⠹⣿⣿⣷⣤⡄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠰⣿⣍⡻⣿⠿⠿⠾⢾⣿⣿⣿⣿⣿⣿⠿⠛⠻⢿⡟⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⣿⣏⣰⣿⣦⣀⠄⠄⢹⣿⣿⣿⣿⣏⡀⠄⣀⣿⣿⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⣫⣭⡄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠸⠿⠶⠶⢿⣿⣿⣿⣿⣿⣅⣖⣼⣿⣿⣿⣯⣽⣭⠉⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⢸⣿⣿⣿⣿⣿⣿⣿⣿⣭⣤⣽⣿⣿⣿⣿⢿⣻⡇⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⣼⣿⣿⣿⣿⣿⣿⡿⢿⡿⣿⢿⣟⣛⣿⣷⣿⣿⣿⣂⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠄I\n"
