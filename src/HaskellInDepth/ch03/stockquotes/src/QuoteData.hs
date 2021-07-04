{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module QuoteData where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField (..))

data QuoteData = QuoteData {
                   day :: Day
                 , volume :: Int    -- ^ total stock volume 
                 , open :: Double   -- ^ opening price
                 , close :: Double  -- ^ closing price 
                 , high :: Double   -- ^ high price of the day
                 , low :: Double    -- ^ low price of the day
                 } deriving (Generic, FromNamedRecord)


instance FromField Day where
    parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

    
data QField = Open | Close | High | Low | Volume
    deriving (Eq, Ord, Show, Enum, Bounded)


-- | coercion to a Double
field2fun :: QField -> QuoteData -> Double
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = fromIntegral . volume

