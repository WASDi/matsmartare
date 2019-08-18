module FetchDbItems where

import           Model
import           ParseDbItem

import           Database.HDBC
import           Database.HDBC.Sqlite3

fetchDbItems :: IO [Item]
fetchDbItems = do
  c <- connectSqlite3 "matsmartare.db"
  select <- prepare c "SELECT * FROM items"
  execute select []
  rows <- fetchAllRows' select
  disconnect c
  return $ parseDbItems rows
