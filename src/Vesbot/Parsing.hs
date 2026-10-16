{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Parsing where

import Vesbot.Logging as Logging (log, LogType(..))

import Discord.Types
import Discord.Internal.Types.Interactions

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.Time
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Data.Scientific (toRealFloat)


intToUTC :: Int64 -> UTCTime
intToUTC = posixSecondsToUTCTime . realToFrac

utcToInt :: UTCTime -> Int64
utcToInt = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

safeHead :: a -> [a] -> a
safeHead d [] = d
safeHead _ (x:xs) = x


parseJSON :: AE.FromJSON a => FilePath -> IO (Maybe a)
parseJSON path = do
  jsonData <- BS.readFile path
  let decoded = AE.decodeStrict jsonData
  case decoded of
    Nothing -> do
      Logging.log Error $ "Error parsing the JSON data in " <> T.pack path <> "."
      return Nothing
    Just d  -> do
      Logging.log Info $ "Successfully parsed the JSON data in " <> T.pack path <> "."
      return (Just d)


-- | Kind of a silly function, date input parsing for the predictions is fairly restrictive right now.
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


-- | Extracting otions from command fields
extractStringOption :: T.Text -> OptionsData -> Maybe T.Text
extractStringOption _ (OptionsDataSubcommands _) = Nothing
extractStringOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueString n (Right v) <- vs, n == optionName] of
    (v:_) -> Just v
    _     -> Nothing

extractIntegerOption :: T.Text -> OptionsData -> Maybe Integer
extractIntegerOption _ (OptionsDataSubcommands _) = Nothing
extractIntegerOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueInteger n (Right v) <- vs, n == optionName] of
    (v:_) -> Just v
    _     -> Nothing

extractNumberOption :: T.Text -> OptionsData -> Maybe Double
extractNumberOption _ (OptionsDataSubcommands _) = Nothing
extractNumberOption optionName (OptionsDataValues vs) =
  case [v | OptionDataValueNumber n (Right v) <- vs, n == optionName] of
    (v:_) -> Just (toRealFloat v)
    _     -> Nothing

extractUserOption :: T.Text -> OptionsData -> Maybe UserId
extractUserOption _ (OptionsDataSubcommands _) = Nothing
extractUserOption optionName (OptionsDataValues vs) =
  case [uid | OptionDataValueUser n uid <- vs, n == optionName] of
    (uid:_) -> Just uid
    _     -> Nothing


-- Helper function that extracts the UserId from a MemberOrUser type
getUserData :: MemberOrUser -> Maybe UserId
getUserData (
  MemberOrUser (
    Left (
      GuildMember
        { memberUser = Just (
            User { userId = uid }
          )
        }
      )
    )
  ) = Just uid

