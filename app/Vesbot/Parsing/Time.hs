module Vesbot.Parsing.Time where

import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Char (isDigit)
import Vesbot.Utils (safeHead)

parseDateInput :: UTCTime -> String -> Maybe UTCTime
parseDateInput currTime inpt = case words inpt of
  ("on":absInpt:timeInpt:timeZoneOrEmpty) ->
    let
      timeZone = parseTimeM False defaultTimeLocale "%Z" (safeHead "UTC" timeZoneOrEmpty)
    in do
      date <- parseTimeM False defaultTimeLocale "%m-%d-%Y" absInpt :: Maybe Day
      time <- parseTimeM False defaultTimeLocale "%R" timeInpt :: Maybe TimeOfDay
      tz <- timeZone
      return (localTimeToUTC tz (LocalTime date time))

  ("in":relInpt:_) -> do
    let letter = dropWhile isDigit relInpt
    timeIncrement <- parseTimeM False defaultTimeLocale (concat ["%", letter, letter]) relInpt :: Maybe NominalDiffTime
    return (addUTCTime timeIncrement currTime)

  _          -> Nothing

formatUTCTime :: UTCTime -> Integer
formatUTCTime = toInteger . floor . utcTimeToPOSIXSeconds
