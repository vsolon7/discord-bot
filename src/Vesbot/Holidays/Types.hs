{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Holidays.Types where

import qualified Data.Text as T
import Data.Time (Day)
import qualified Data.Aeson as AE
import Data.Aeson.Types as AE ((.:), typeMismatch, prependFailure)

data Holiday = Holiday
  { holidayName :: T.Text
  , holidayBlurb :: T.Text
  , holidayDate :: Day
  } deriving Show
  
  
instance AE.FromJSON Holiday where
  parseJSON (AE.Object v) =
    Holiday
      <$> (v .: "holidayName")
      <*> (v .: "holidayBlurb")
      <*> (read <$> (v .: "holidayDate"))

  parseJSON invalid =
    prependFailure "parsing Holiday failed, "
      (typeMismatch "Object" invalid)
