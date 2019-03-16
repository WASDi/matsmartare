module Main where

import Model
import FetchDbItems
import FetchWebItems
import InsertUpdateDbItem
import PriceChangeDetector (PriceChange, parsePriceChanges)

import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)

import Database.HDBC
import Database.HDBC.Sqlite3

type ItemMap = Map.Map Int Item

main :: IO ()
main = do
    webItemsOrFail <- fetchWebItems
    dbItems <- fetchDbItems
    
    case webItemsOrFail of
        Left  errorMsg -> putStrLn errorMsg
        Right webItems -> process dbItems webItems

process :: [Item] -> [Item] -> IO ()
process dbItems webItems = do
    let dbMap = itemMap dbItems
    let webMap = itemMap webItems
    let inBoth = Map.elems $ Map.intersectionWith (,) dbMap webMap
    let newItems = Map.elems $ Map.difference webMap dbMap
    let priceChanges = parsePriceChanges inBoth
    
    now_timestamp <- unixTimestamp
    conn <- connectSqlite3 "matsmartare.db"
    
    putStrLn $ "num dbItems " ++ (show $ length dbItems)
    putStrLn $ "num webItems " ++ (show $ length webItems)
    putStrLn $ "num newItems " ++ (show $ length newItems)
    putStrLn $ "num priceChanges " ++ (show $ length priceChanges)
    
    insertNewItems conn now_timestamp newItems
    updateItems conn now_timestamp (map snd inBoth)
    insertPriceChanges conn now_timestamp priceChanges
    insertUpdateLog conn now_timestamp (length webItems, length newItems)
    
    commit conn
    disconnect conn

insertUpdateLog :: Connection -> Int -> (Int, Int) -> IO ()
insertUpdateLog conn now_timestamp (num_web_items, num_new_items) = do
    stmt <- prepare conn "INSERT INTO update_logs (when_timestamp, num_web_items, num_new_items) VALUES (?, ?, ?)"
    execute stmt [toSql now_timestamp, toSql num_web_items, toSql num_new_items]
    return ()

unixTimestamp :: IO Int
unixTimestamp = fmap round getPOSIXTime

itemMap :: [Item] -> ItemMap
itemMap = Map.fromList . map withId
    where
        withId :: Item -> (Int, Item)
        withId item = (id' item, item)
