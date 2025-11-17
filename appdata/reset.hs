import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  time <- getCurrentTime
  let time1 = T.pack . show $ time
  TIO.writeFile "savedtime" time1

plural :: Int -> String -> String
plural n str = if (n /= 1) then str ++ "s" else str
