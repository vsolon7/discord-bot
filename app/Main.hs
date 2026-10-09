{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Vesbot.Handler as Handler (eventHandler)
import Vesbot.Logging as Logging (echo)
import Vesbot.Config as Config (initBot, cfgApiToken)

import Discord
import Discord.Types


main :: IO ()
main = do
  (env, cfg) <- Config.initBot

  botTerminationError <-
    runDiscord $
      def
        { discordToken = cfgApiToken cfg
        , discordOnStart = return ()
        , discordOnEvent = Handler.eventHandler cfg env
        , discordOnEnd = do
            Logging.echo "Bot has disconnected. Cleaning up..."
        , discordGatewayIntent = def {gatewayIntentMessageContent = True}
        }

  echo $ "A fatal error occurred: " <> botTerminationError
