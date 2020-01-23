module FetchWebItems where

import           Model
import           ParseWebItem
import           ParseWebRoutes
import           RawJson
import           TextUtil

import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

import           Data.Either             (lefts, rights)

import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           System.Process          (readProcess)

onlineMode :: Bool
onlineMode = True

getFromWebOrFile :: String -> FilePath -> IO T.Text
getFromWebOrFile url file =
  if onlineMode
    then httpRequest url
    else TIO.readFile file

httpRequest :: String -> IO T.Text
httpRequest url = T.pack <$> readProcess "wget" ["-O", "-", "--quiet", url] []

getProducts :: IO T.Text
getProducts =
  getFromWebOrFile
    "https://api.matsmart.se/api/v1.0/product-displays?market=SE"
    "git_ignore/jsons/latest/products.json"

getRoutes :: IO T.Text
getRoutes =
  getFromWebOrFile "https://api.matsmart.se/api/v1.0/routes?market=SE" "git_ignore/jsons/latest/routes.json"

getIndexPage :: IO T.Text
getIndexPage = getFromWebOrFile "https://www.matsmart.se/" "git_ignore/index.html"

extractPreloadedJson :: T.Text -> Maybe T.Text
extractPreloadedJson = extract (T.pack "<script>window.__PRELOADED_STATE__=") (T.pack "</script>")

fetchPreloadedJson :: IO (Maybe T.Text)
fetchPreloadedJson = fmap extractPreloadedJson getIndexPage

fetchWebItems :: IO (Either String [Item])
fetchWebItems = do
  products <- getProducts
  routes <- getRoutes
  case parseRoutes routes of
    Left errorMsg -> return $ Left errorMsg
    Right urlMap ->
      case parseRawJson products of
        Left errorMsg  -> return $ Left errorMsg
        Right jsonRoot -> return $ parse' urlMap jsonRoot

parse' :: UrlMap -> RawJsonRoot -> Either String [Item]
parse' urlMap json =
  let parseResult = map (parseRaw urlMap) (_data json)
      fails = lefts parseResult
      items = rights parseResult
   in if null fails
        then Right items
        else Left (unlines fails)
