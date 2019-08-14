module Categories.ParseCategories
       (
         decodeCategoriesJson,
         parseCategories,
         Category (Category)
       )
       where

import Categories.CategoriesJson

import Data.Aeson

import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BL

data Category = Category
    { id'  :: Int
    , url  :: String
    , name :: String
    } deriving Show

decodeCategoriesJson :: T.Text -> Either String CategoriesJsonRoot
decodeCategoriesJson = eitherDecode . BL.fromStrict . TE.encodeUtf8

parseCategories :: CategoriesJsonRoot -> [Category]
parseCategories = map toCategory . Map.elems . _category . _categoryState

toCategory :: CategoryJson -> Category
toCategory (CategoryJson id' label') = Category id' "DEPRECATED" label'
