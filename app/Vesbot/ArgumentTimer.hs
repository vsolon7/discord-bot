module Vesbot.ArgumentTimer where

import Data.Time
import Vesbot.Utils
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


getSavedTime :: FilePath -> IO UTCTime
getSavedTime path = do
  time <- TIO.readFile path
  return (read . T.unpack $ time)

getTimeDiff :: FilePath -> IO NominalDiffTime
getTimeDiff path = do
  prevTime <- TIO.readFile path
  let prevTime1 = read . T.unpack $ prevTime
  currTime <- getCurrentTime
  return $ diffUTCTime currTime prevTime1

setSavedTime :: FilePath -> UTCTime -> IO ()
setSavedTime path time = do
  TIO.writeFile path (showT time)

formatDiffTime :: NominalDiffTime -> String
formatDiffTime time = show days ++ " " ++ plural days "day" ++ ", "
                   ++ show hours ++ " " ++ plural hours "hour" ++ ", "
                   ++ show hours ++ " " ++ plural minutes "minute" ++ ", "
                   ++ show hours ++ " " ++ plural seconds "second"
  where
    (intTime, _) = properFraction time
    (days, [seconds, minutes, hours]) = mapAccumL divMod intTime [60, 60, 24]

    plural :: Int -> String -> String
    plural n str = if n /= 1 then str ++ "s" else str

