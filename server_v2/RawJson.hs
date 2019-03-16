{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module RawJson where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types

import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

parseDrop1 = defaultOptions { fieldLabelModifier = drop 1 }

data RawJsonRoot = RawJsonRoot
    { _count :: Int
    , _data  :: [RawItem]
    } deriving (Show, Generic)

data RawItem = RawItem
    { _id         :: Int
    , _label      :: String
    , _products   :: [RawProducts]
    , _categories :: [String]
    , _created    :: String
    } deriving (Show, Generic)

data RawProducts = RawProducts
    { _best_before :: Maybe String
    , _prices      :: RawPrices
    , _images      :: [RawImages]
    } deriving (Show, Generic)

data RawPrices = RawPrices
    { _old_price   :: Maybe RawPrice
    , _price       :: RawPrice
    , _price_table :: Maybe RawPrice
    } deriving (Show, Generic)

newtype RawPrice = RawPrice
    { _amount :: String
    } deriving (Show, Generic)

newtype RawImages = RawImages
    { _styles :: RawStyles
    } deriving (Show, Generic)

newtype RawStyles = RawStyles
    { _product_teaser :: String
    } deriving (Show, Generic)

instance FromJSON RawJsonRoot where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON RawItem where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON RawProducts where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON RawPrices where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON RawPrice where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON RawImages where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON RawStyles where
  parseJSON = genericParseJSON parseDrop1
  
parseRawJson :: T.Text -> Either String RawJsonRoot
parseRawJson = eitherDecode . BL.fromStrict . TE.encodeUtf8

-- theFile = BL.readFile "jsons/small.json"
-- fmap decode theFile :: IO (Maybe JsonRoot)
-- eitherDecode för att se felmeddelanden !!!
-- fmap ((fmap (head . _data)) . decode) theFile :: IO (Maybe Item)
-- firstItem = theFile >>= return . decode >>= return . fmap (head . _data)

