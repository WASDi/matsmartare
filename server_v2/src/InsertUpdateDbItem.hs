module InsertUpdateDbItem where

import           Model
import           PriceChangeDetector

import           Categories.ParseCategories (Category (Category))

import           Database.HDBC
import           Database.HDBC.Sqlite3

sql_insert =
  "INSERT INTO items (id, categories, url, img_url, name, price, discount, best_before, first_seen, last_seen) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

sql_update =
  "UPDATE items SET categories = ?, img_url = ?, price = ?, discount = ?, best_before = ?, last_seen = ? WHERE id = ?"

sql_price_change = "INSERT INTO price_changes (item_id, price_before, price_after, created) VALUES (?, ?, ?, ?)"

insertNewItems :: Connection -> Int -> [Item] -> IO ()
insertNewItems conn now_timestamp items = do
  stmt <- prepare conn sql_insert
  mapM_ (execute stmt . insertValues now_timestamp) items

updateItems :: Connection -> Int -> [Item] -> IO ()
updateItems conn now_timestamp items = do
  stmt <- prepare conn sql_update
  mapM_ (execute stmt . updateValues now_timestamp) items

insertPriceChanges :: Connection -> Int -> [PriceChange] -> IO ()
insertPriceChanges conn now_timestamp priceChanges = do
  stmt <- prepare conn sql_price_change
  mapM_ (execute stmt . priceChangeValues now_timestamp) priceChanges

insertValues :: Int -> Item -> [SqlValue]
insertValues now_timestamp item =
  [ toSql (id' item)
  , toSql (categoriesToString item)
  , toSql (url item)
  , toSql (img_url item)
  , toSql (name item)
  , toSql (new_price item)
  , toSql (discount_percentage item)
  , toSql (best_before item)
  , toSql now_timestamp
  , toSql now_timestamp
  ]

updateValues :: Int -> Item -> [SqlValue]
updateValues now_timestamp item =
  [ toSql (categoriesToString item)
  , toSql (img_url item)
  , toSql (new_price item)
  , toSql (discount_percentage item)
  , toSql (best_before item)
  , toSql now_timestamp
  , toSql (id' item)
  ]

priceChangeValues :: Int -> PriceChange -> [SqlValue]
priceChangeValues now_timestamp priceChange =
  [toSql (item_id priceChange), toSql (before priceChange), toSql (after priceChange), toSql now_timestamp]

updateCategories :: Connection -> [Category] -> IO ()
updateCategories conn categories = do
  wipe <- prepare conn "DELETE FROM categories"
  stmt <- prepare conn "INSERT INTO categories (id, url, name) VALUES (?, ?, ?)"
  execute wipe []
  mapM_ (execute stmt . categoryValues) categories

categoryValues :: Category -> [SqlValue]
categoryValues (Category id' url name) = [toSql id', toSql url, toSql name]

insertUpdateLog :: Connection -> Int -> (Int, Int) -> IO ()
insertUpdateLog conn now_timestamp (num_web_items, num_new_items) = do
  stmt <- prepare conn "INSERT INTO update_logs (when_timestamp, num_web_items, num_new_items) VALUES (?, ?, ?)"
  execute stmt [toSql now_timestamp, toSql num_web_items, toSql num_new_items]
  return ()
