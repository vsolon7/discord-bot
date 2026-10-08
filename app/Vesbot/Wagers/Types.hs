module Vesbot.Wagers.Types where

import Discord.Types
import Data.Time
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL

import Vesbot.Utils (showT)
import Vesbot.Parsing.Time (formatUTCTime)


data WagerCommandData = WagerCommandData
  { wagerContent :: T.Text
  , wagerBet :: Double
  , wagerOdds :: Maybe Rational
  , wagerDueDate :: UTCTime
  , wagerOfferingUserId :: UserId
  , wagerGuild :: GuildId
  , wagerChannel :: ChannelId
  } deriving Show

data AcceptedWager = AcceptedWager
  { wagerCommandData :: WagerCommandData
  , wagerAcceptingUser :: UserId
  , wagerAcceptedDate :: UTCTime
  , wagerOfferMessageId :: MessageId
  } deriving Show

instance SQL.ToRow AcceptedWager where
  toRow (AcceptedWager initialOffer taker acceptDate wmid) =
    [ SQL.SQLText (wagerContent initialOffer)
    , SQL.SQLFloat (wagerBet initialOffer)
    , fromMaybe SQL.SQLNull (fmap (SQL.SQLFloat . fromRational) (wagerOdds initialOffer))
    , SQL.SQLText (showT . formatUTCTime . wagerDueDate $ initialOffer)
    , SQL.SQLText (showT . wagerOfferingUserId $ initialOffer)
    , SQL.SQLText (showT . wagerGuild $ initialOffer)
    , SQL.SQLText (showT . wagerChannel $ initialOffer)
    , SQL.SQLText (showT taker)
    , SQL.SQLText (showT acceptDate)
    , SQL.SQLText (showT wmid)
    ]
