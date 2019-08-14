{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.CategoriesJson where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Scientific as DS
import qualified Data.Text as T

newtype CategoriesJsonRoot = CategoriesJsonRoot
  { _categoryState :: CategoryState
  } deriving (Show, Generic)

newtype CategoryState = CategoryState
  { _category :: Map.Map String CategoryJson
  } deriving (Show, Generic)

data CategoryJson = CategoryJson
  { _id :: Int
  , _label :: String
  } deriving (Show, Generic)

parseDrop1 = defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON CategoriesJsonRoot where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON CategoryState where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON CategoryJson where
  parseJSON = withObject "CategoryJson" parseCategoryJson

parseCategoryJson :: Object -> Parser CategoryJson
parseCategoryJson =
  \obj -> do
    id' <-
      case HM.lookup "id" obj of
        Just x -> intStringParser x
        Nothing -> fail "no field 'id'"
    label' <- obj .: "label"
    return $ CategoryJson id' label'

intStringParser :: Value -> Parser Int
intStringParser (Number n) = forceInt n
intStringParser (String s) = return (read $ T.unpack s)
intStringParser v = fail $ "Unexpected type for " ++ (show v)

forceInt :: DS.Scientific -> Parser Int
forceInt x =
  case DS.toBoundedInteger x of
    Just int -> return int
    Nothing -> fail $ "Not integer: " ++ (show x)
