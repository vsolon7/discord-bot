{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Logging
  ( Vesbot.Logging.log
  , LogType(..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (putStrLn, appendFile)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale, FormatTime)
import Control.Monad (when, void)
import Control.Concurrent (forkIO)

data LogType = Always | Error | Warn | Info
  deriving (Eq, Ord)

instance Show LogType where
  show v = case v of
    Always -> "LOG"
    Info -> "INFO"
    Warn -> "WARN"
    Error -> "ERROR"

_LOGFILE :: FilePath
_LOGFILE = "log.out"
_LOGFILE_VERBOSITY_LEVEL :: LogType
_LOGFILE_VERBOSITY_LEVEL = Warn
_TERMINAL_VERBOSITY_LEVEL :: LogType
_TERMINAL_VERBOSITY_LEVEL = Info

showTime :: FormatTime t => t -> T.Text
showTime t = T.pack $ formatTime defaultTimeLocale "%D %T %Z" t

log :: MonadIO m => LogType -> T.Text -> m ()
log v msg = do
  when (_TERMINAL_VERBOSITY_LEVEL >= v) $ do
    liftIO . TIO.putStrLn $ "[" <> T.show v <> "] " <> msg
  when (_LOGFILE_VERBOSITY_LEVEL >= v) $ void . liftIO . forkIO $ do
      now <- getCurrentTime
      let out = "[" <> T.show v <> " | "  <> showTime now <> "] " <> msg <> "\n"
      TIO.appendFile _LOGFILE out
