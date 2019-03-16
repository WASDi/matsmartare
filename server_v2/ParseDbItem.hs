module ParseDbItem where

import Model

import Database.HDBC

parseDbItems :: [[SqlValue]] -> [Item]
parseDbItems = map parseDbItem

parseDbItem :: [SqlValue] -> Item
parseDbItem row = let id          = row !! 0
                      categories  = row !! 1
                      url         = row !! 2
                      img_url     = row !! 3
                      name        = row !! 4
                      price       = row !! 5
                      discount    = row !! 6
                      best_before = row !! 7
                  in  Item (fromSql id)
                           (parseCategories categories)
                           (fromSql url)
                           (fromSql img_url)
                           (fromSql name)
                           (fromSql price)
                           (fromSql discount)
                           (fromSql best_before)

parseCategories :: SqlValue -> [Int]
parseCategories = fmap read . words . fmap (\c -> if c == ',' then ' ' else c) . fromSql
