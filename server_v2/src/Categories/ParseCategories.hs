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
parseCategories json = let routeItems  = filter isCategory . _routes . _routeState $ json
                           alias2label = aliasLabelMap json
                           ff = fff alias2label
                        in catMaybes $ map (parseCategory . ff) routeItems
    where
        fff alias2label (RouteItem id' alias' _) = (id', alias', Map.lookup (alias') alias2label)

parseCategory :: (String, String, Maybe String) -> Maybe Category
parseCategory (id', alias, (Just name)) = Just $ Category (read id') ('/':alias) name
parseCategory (_, _, Nothing)           = Nothing

aliasLabelMap :: CategoriesJsonRoot -> Map.Map String String
aliasLabelMap = Map.fromList . map aliasLabelMap' . _menu . _menuState

aliasLabelMap' :: MenuItem -> (String, String)
aliasLabelMap' (MenuItem alias label) = (alias, label)

isCategory :: RouteItem -> Bool
isCategory (RouteItem _ _ resource) = resource `elem` ["categories", "tags"]
