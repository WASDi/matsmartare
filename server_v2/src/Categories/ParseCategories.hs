module Categories.ParseCategories
       (
         decodeCategoriesJson,
         parseCategories
       )
       where

import Categories.CategoriesJson

import Data.Aeson

import qualified Data.Map.Strict as Map

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as BL

decodeCategoriesJson :: T.Text -> Either String CategoriesJsonRoot
decodeCategoriesJson = eitherDecode . BL.fromStrict . TE.encodeUtf8

parseCategories :: CategoriesJsonRoot -> [(Int,String)]
parseCategories json = Map.toList $ Map.mapMaybe (flip Map.lookup (aliasLabelMap json)) (idAliasMap json)

aliasLabelMap :: CategoriesJsonRoot -> Map.Map String String
aliasLabelMap = Map.fromList . map aliasLabelMap' . _menu . _menuState

aliasLabelMap' :: MenuItem -> (String, String)
aliasLabelMap' (MenuItem alias label) = (alias, label)

idAliasMap :: CategoriesJsonRoot -> Map.Map Int String
idAliasMap = Map.fromList . map idAliasMap' . filter isCategory . _routes . _routeState

idAliasMap' :: RouteItem -> (Int, String)
idAliasMap' (RouteItem _id alias _) = (read _id, alias)

isCategory :: RouteItem -> Bool
isCategory (RouteItem _ _ resource) = resource `elem` ["categories", "tags"]
