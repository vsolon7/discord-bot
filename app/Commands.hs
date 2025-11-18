{-# LANGUAGE OverloadedStrings #-}
module Commands where

import Discord
import Discord.Interactions
import Discord.Types (messageChannelId, messageId, Message)
import UnliftIO (liftIO)
import UnliftIO.Concurrent
import Data.Text (Text)
import Control.Monad (void)
import qualified Discord.Requests as R
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Utils

data SlashCommand = SlashCommand
  { commandName :: Text
  , commandRegistration :: Maybe CreateApplicationCommand
  , commandHandler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

data KeywordResponse = KeywordResponse
  { responseName :: Text
  , responseKeyword :: Text
  , responseHandler :: Message -> DiscordHandler()
  }

responseFromJSONTemplate :: Response -> KeywordResponse
responseFromJSONTemplate res = KeywordResponse
  { responseName = rName res
  , responseKeyword = rKeyword res
  , responseHandler = \mess -> do
      void . restCall $
        R.CreateReaction
          (messageChannelId mess, messageId mess)
          (rEmojiReactId res)

      threadDelay (2 * 10^(5 :: Int))

      void . restCall $
        R.CreateMessage
          (messageChannelId mess)
          (rMessageReply res)
  }

slashCommandFromTemplate :: Text    -> -- Slash Command Name
                            Text    -> -- Registration Description
                            IO Text -> -- Text diplayed in the interaction response, possibly obtained with IO
                            SlashCommand
slashCommandFromTemplate
  name
  regDesc
  statefulText
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

_ARGTIMER_FILEPATH = "appdata/savedtime"

-- List of slash commands to register
mySlashCommands :: [SlashCommand]
mySlashCommands = [ping, getCurrTime, resetArgCounter, viewArgCounter, printGiantGlorp]

myKeywordResponses :: [KeywordResponse]
myKeywordResponses = [responseFromJSONTemplate testResponseFromJSON]

ping :: SlashCommand
ping = slashCommandFromTemplate
         ("ping")
         ("Responds 'pong'")
         (return "pong!")


getCurrTime :: SlashCommand
getCurrTime = slashCommandFromTemplate
                ("currtime")
                ("Displays the Current time in UTC.")
                (do x <- getCurrentTime; return $ "The current time (in UTC) is " <> showT x <> ".")


resetArgCounter :: SlashCommand
resetArgCounter = slashCommandFromTemplate
                    ("resetac")
                    ("Resets the time since the last autistic argument.")
                    (saveTimeIO)
  where
    saveTimeIO :: IO Text
    saveTimeIO = do
      currTime <- getCurrentTime
      setSavedTime _ARGTIMER_FILEPATH currTime
      return $ T.pack "Time since the last autistic argument: 0 days"


viewArgCounter :: SlashCommand
viewArgCounter = slashCommandFromTemplate
                   ("ac")
                   ("Gives the time since the last autistic argument.")
                   (timeDiffIO)
  where
    timeDiffIO = do
      timeDiff <- getTimeDiff _ARGTIMER_FILEPATH
      return $ "Time since the last autistic argument: " <> (T.pack $ formatDiffTime timeDiff) <> "."


printGiantGlorp :: SlashCommand
printGiantGlorp = slashCommandFromTemplate
                    ("giantglorp")
                    ("Prints a giant glorp.")
                    (return . T.pack $ giantGlorp)

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

testResponseFromJSON :: Response
testResponseFromJSON = Response
                      { rName = "testResponse"
                      , rKeyword = "test"
                      , rEmojiReactId = "eyes"
                      , rMessageReply = "I have read the message."
                      }
