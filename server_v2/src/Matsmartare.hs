module Matsmartare where

import Model
import FetchDbItems
import FetchWebItems
import qualified InsertUpdateDbItem as DB
import PriceChangeDetector (PriceChange, parsePriceChanges)
import Categories.CategoriesJson
import Categories.ParseCategories

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)

import Database.HDBC
import Database.HDBC.Sqlite3

type ItemMap = Map.Map Int Item

main :: IO ()
main = do
    dbItems <- fetchDbItems
    webItemsOrFail <- fetchWebItems
    preloadedJson <- fetchPreloadedJson
    
    case webItemsOrFail of
        Left  errorMsg -> putStrLn errorMsg >> logError errorMsg
        Right webItems -> process dbItems webItems preloadedJson

logError :: String -> IO ()
logError = appendFile "/tmp/EXTRA_BIT" . (++ "\n")

process :: [Item] -> [Item] -> Maybe T.Text -> IO ()
process dbItems webItems categories = do
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
    
    DB.insertNewItems conn now_timestamp newItems
    DB.updateItems conn now_timestamp (map snd inBoth)
    DB.insertPriceChanges conn now_timestamp priceChanges
    DB.insertUpdateLog conn now_timestamp (length webItems, length newItems)

    numCategories <- processCategories conn categories
    putStrLn $ "num categories " ++ (show numCategories)
    
    commit conn
    disconnect conn

processCategories :: Connection -> Maybe T.Text -> IO Int
processCategories conn Nothing               = logError "categories: Nothing" >> return 0
processCategories conn (Just categoriesText) = processCategories' conn $ decodeCategoriesJson categoriesText

processCategories' :: Connection -> Either String CategoriesJsonRoot -> IO Int
processCategories' conn (Left err)  = logError ("categories error: " ++ err) >> return 0
processCategories' conn (Right cat) = do
    let categories = parseCategories cat
    let numCategories = length categories
    if numCategories == 0
        then logError "categories empty"
        else DB.updateCategories conn categories
    return numCategories

unixTimestamp :: IO Int
unixTimestamp = fmap round getPOSIXTime

itemMap :: [Item] -> ItemMap
itemMap = Map.fromList . map withId
    where
        withId :: Item -> (Int, Item)
        withId item = (id' item, item)
