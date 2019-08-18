module TestParseCategories where

import           Categories.CategoriesJson
import qualified Categories.ParseCategories as ParseCat
import qualified FetchWebItems

import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE

testParseCategories :: IO ()
testParseCategories = do
  indexFile <- TIO.readFile "git_ignore/index.html"
  case parseIt indexFile of
    Left error -> putStrLn $ "Shit! " ++ error
    Right categories -> putStrLn "Parse successful!" >> putStrLn (show categories)

parseIt :: T.Text -> Either String [ParseCat.Category]
parseIt indexHtml = do
  preloadedJson <- maybeToRight "big fail!" (FetchWebItems.extractPreloadedJson indexHtml)
  categoriesJson <- ParseCat.decodeCategoriesJson preloadedJson
  return $ ParseCat.parseCategories categoriesJson

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight x Nothing  = Left x
maybeToRight _ (Just x) = Right x
