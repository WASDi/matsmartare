module ParseDbItem where

import           Model

import           Database.HDBC

parseDbItems :: [[SqlValue]] -> [Item]
parseDbItems = map parseDbItem

parseDbItem :: [SqlValue] -> Item
parseDbItem row =
  let [id, categories, url, img_url, name, price, discount, best_before, max_purchase] = row
   in Item
        (fromSql id)
        (parseCategories categories)
        (fromSql url)
        (fromSql img_url)
        (fromSql name)
        (fromSql price)
        (fromSql discount)
        (fromSql best_before)
        (fromSql max_purchase)

parseCategories :: SqlValue -> [Int]
parseCategories =
  fmap read .
  words .
  fmap
    (\c ->
       if c == ','
         then ' '
         else c) .
  fromSql
