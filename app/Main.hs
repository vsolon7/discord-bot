{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Vesbot.Handler as Handler (eventHandler)
import Vesbot.Handler.Actions as Handler (actionHandler)
import Vesbot.Logging as Logging (echo)
import Vesbot.Config as Config (initBot, Env(..))
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
        , discordOnLog = Logging.echo
        , discordOnEnd = do
            Logging.echo "\nBot has disconnected. Cleaning up..."
            closeScheduler (envScheduler env)
        , discordGatewayIntent = def {gatewayIntentMessageContent = True}
        }

  echo $ "A fatal error occurred: " <> botTerminationError
