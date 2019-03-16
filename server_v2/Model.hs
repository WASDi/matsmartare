module Model where

import Data.List (intercalate)

data Item = Item
    { id'                 :: Int
    , categories          :: [Int]
    , url                 :: String
    , img_url             :: String
    , name                :: String
    , new_price           :: Double
    , discount_percentage :: Int
    
    , best_before         :: Maybe String -- yyyy-mm-dd
        
    } deriving Show

categoriesToString :: Item -> String
categoriesToString = intercalate "," . map show . categories
