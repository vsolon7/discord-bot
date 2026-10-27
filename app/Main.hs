{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Vesbot.Handler as Handler (eventHandler)
import Vesbot.Handler.Actions as Handler (actionHandler)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config as Config (initBot, Env(..))
import Vesbot.Database as Database (closeDBConnection)
import JSONScheduler.Scheduler (runScheduler, closeScheduler)

import Discord
import Discord.Types


main :: IO ()
main = do
  (tok, env, cfg) <- Config.initBot

  botTerminationError <-
    runDiscord $
      def
        { discordToken = tok
        , discordForkThreadForEvents = True
        , discordOnStart = runScheduler (envScheduler env) (actionHandler env cfg)
        , discordOnEvent = Handler.eventHandler cfg env
        , discordOnLog = Logging.log Error
        , discordOnEnd = do
            Logging.log Always "Bot has disconnected. Cleaning up..."
            closeScheduler (envScheduler env)
            closeDBConnection (envDBConnection env)
        , discordGatewayIntent = def {gatewayIntentMessageContent = True}
        }

  Logging.log Error $ "A fatal error occurred: " <> botTerminationError
