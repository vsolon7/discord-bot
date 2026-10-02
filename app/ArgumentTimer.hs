module ArgumentTimer where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import Data.Time
import Utils
import Data.List

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

