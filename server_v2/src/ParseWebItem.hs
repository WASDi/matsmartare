module ParseWebItem where

import           Model
import           TimeUtil

import           ParseWebRoutes (UrlMap)
import qualified RawJson        as RJ

import           Data.Map       (lookup)
import           Data.Maybe     (fromMaybe)

parseRaw :: UrlMap -> RJ.RawItem -> Either String Item
parseRaw urlMap rawItem =
  case parseRaw' urlMap rawItem of
    Just item -> Right item
    Nothing -> Left $ "For some reason failed to parse: " ++ show (RJ._id rawItem)

parseRaw' :: UrlMap -> RJ.RawItem -> Maybe Item
parseRaw' urlMap (RJ.RawItem id'' label rawProducts categories tags _) = do
  rawProduct <- oneOrNothing rawProducts
  let url = fromMaybe "#NO_URL" $ Data.Map.lookup id'' urlMap
  let prices = RJ._prices rawProduct
  let new_price = parseNewPrice prices
  let old_price = maybe new_price parseAmount (RJ._old_price prices)
  let img_url = parseImage rawProduct
  let best_before_timestamp = formatYMD <$> RJ.getIfInt (RJ._best_before rawProduct)
  let max_purchase = RJ.getIfInt (RJ._max_purchase rawProduct)
  return $
    Item
      id''
      (categories ++ tags)
      ('/' : url)
      img_url
      label
      new_price
      (discountPercentage new_price old_price)
      best_before_timestamp
      max_purchase

discountPercentage :: Double -> Double -> Int
discountPercentage new old = round $ (1 - new / old) * 100

parseImage :: RJ.RawProducts -> String
parseImage rawProduct =
  case oneOrNothing (RJ._images rawProduct) of
    Just rawImage -> takeWhile ('?' /=) $ RJ._product_teaser $ RJ._styles rawImage
    Nothing -> "https://www.matsmart.se/static/media/logo.474f2a56.svg"

oneOrNothing :: [a] -> Maybe a
oneOrNothing [x] = Just x
oneOrNothing _   = Nothing

parseNewPrice :: RJ.RawPrices -> Double
parseNewPrice (RJ.RawPrices _ price bulkPrice) = parseAmount $ fromMaybe price bulkPrice

parseAmount :: RJ.RawPrice -> Double
parseAmount = (/ 100) . fromIntegral . RJ._amount
