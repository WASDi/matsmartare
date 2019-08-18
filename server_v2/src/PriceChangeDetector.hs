module PriceChangeDetector where

import           Model

data PriceChange =
  PriceChange
    { item_id :: Int
    , before  :: Double
    , after   :: Double
    }
  deriving (Show)

parsePriceChanges :: [(Item, Item)] -> [PriceChange]
parsePriceChanges = map toPriceChange . filter hasPriceDiff

hasPriceDiff :: (Item, Item) -> Bool
hasPriceDiff (before, after) = new_price before /= new_price after

toPriceChange :: (Item, Item) -> PriceChange
toPriceChange (before, after) = PriceChange (id' before) (new_price before) (new_price after)
