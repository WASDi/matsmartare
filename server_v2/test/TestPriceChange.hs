module TestPriceChange where

import           ParseModel
import           ParseRoutes
import           PriceChangeDetector
import           RawJson

import           Data.Maybe            (catMaybes)

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Database.HDBC
import           Database.HDBC.Sqlite3

main :: IO ()
main = do
  before_products <- TIO.readFile "jsons/2mars2019/products.json"
  before_routes <- TIO.readFile "jsons/2mars2019/routes.json"
  after_products <- TIO.readFile "jsons/latest/products.json"
  after_routes <- TIO.readFile "jsons/latest/routes.json"
  case parseItems before_products before_routes of
    Left errorMsg -> putStrLn $ "1: " ++ errorMsg
    Right before_items ->
      case parseItems after_products after_routes of
        Left errorMsg -> putStrLn $ "2: " ++ errorMsg
        Right after_items -> insertPriceChangesInDb $ priceChanges before_items after_items

parseItems :: T.Text -> T.Text -> Either String [Item]
parseItems products routes = do
  routes' <- parseRoutes routes
  products' <- parseRawJson products
  return . catMaybes $ map (parseRaw routes') (_data products')

insertPriceChangesInDb :: [PriceChange] -> IO ()
insertPriceChangesInDb priceChanges = do
  mapM_ (putStrLn . show) priceChanges
  c <- connectSqlite3 "matsmartare.db"
  stmt <-
    prepare
      c
      "INSERT INTO price_changes (item_id, price_before, price_after, created) VALUES (?, ?, ?, current_timestamp);"
  mapM_ (insertPriceChange stmt) priceChanges
  commit c

insertPriceChange :: Statement -> PriceChange -> IO Integer
insertPriceChange stmt priceChange =
  execute stmt [toSql (item_id priceChange), toSql (before priceChange), toSql (after priceChange)]
