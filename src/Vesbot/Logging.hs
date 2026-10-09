{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Logging where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (putStrLn)


echo :: MonadIO m => T.Text -> m ()
echo = liftIO . TIO.putStrLn
