{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Vesbot.Handler as Handler (eventHandler)
import Vesbot.Logging as Logging (echo)
import Vesbot.Config as Config (initBot, cfgApiToken, envDBConnection, envNotifierControl)
import Vesbot.Notifications as Notifications (startNotifier)

import Discord
import Discord.Types


main :: IO ()
main = do
  (env, cfg) <- Config.initBot

  botTerminationError <-
    runDiscord $
      def
        { discordToken = cfgApiToken cfg
        , discordForkThreadForEvents = True
        , discordOnStart = Notifications.startNotifier (envDBConnection env) (envNotifierControl env)
        , discordOnEvent = Handler.eventHandler cfg env
        , discordOnLog = Logging.echo
        , discordOnEnd = do
            Logging.echo "Bot has disconnected. Cleaning up..."
        , discordGatewayIntent = def {gatewayIntentMessageContent = True}
        }

  echo $ "A fatal error occurred: " <> botTerminationError
