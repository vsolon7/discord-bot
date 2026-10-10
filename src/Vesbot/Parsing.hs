{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Parsing where

import Vesbot.Logging as Logging (echo)

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.Text as T (pack)

import Data.Time (UTCTime)
import Data.Time.Clock (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Int (Int64)

parseJSON :: AE.FromJSON a => FilePath -> IO (Maybe a)
parseJSON path = do
  jsonData <- BS.readFile path
  let decoded = AE.decodeStrict jsonData
  case decoded of
    Nothing -> do
      Logging.echo $ "Error parsing the JSON Data in " <> T.pack path <> "."
      return Nothing
    Just d  -> return (Just d)


intToUTC :: Int64 -> UTCTime
intToUTC = posixSecondsToUTCTime . realToFrac

utcToInt :: UTCTime -> Int64
utcToInt = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
