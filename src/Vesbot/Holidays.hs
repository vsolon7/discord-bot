module Vesbot.Holidays where

import Vesbot.Parsing (parseJSON)
import Vesbot.Holidays.Types (Holiday)
import Vesbot.Holidays.Database

import Data.Maybe (fromMaybe)

readHolidays :: FilePath -> IO [Holiday]
readHolidays file = do
  hs <- parseJSON file :: IO (Maybe [Holiday])
  return (fromMaybe [] hs)
