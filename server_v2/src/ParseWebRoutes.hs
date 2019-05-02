{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module ParseWebRoutes
       (
         parseRoutes,
         UrlMap
       )
       where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Map -- TODO qualified as Map

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types

type UrlMap = Map Int String

parseRoutes :: T.Text -> Either String UrlMap
parseRoutes input = (decodeIt input) >>= (return . parseRoutes')

decodeIt :: T.Text -> Either String JsonRoot
decodeIt = eitherDecode . BL.fromStrict . TE.encodeUtf8

parseRoutes' :: JsonRoot -> UrlMap
parseRoutes' = fromList . toKeyValue

toKeyValue :: JsonRoot -> [(Int, String)]
toKeyValue = Prelude.map toKeyValue' . Prelude.filter ((==) "product-displays" . _resource) . _data

toKeyValue' :: Route -> (Int, String)
toKeyValue' (Route id' url _) = (read id', url)

newtype JsonRoot = JsonRoot
    { _data  :: [Route]
    } deriving (Show, Generic)

data Route = Route
    { _id       :: String
    , _alias    :: String
    , _resource :: String
    } deriving (Show, Generic)

instance FromJSON JsonRoot where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON Route where
  parseJSON = genericParseJSON parseDrop1

parseDrop1 = defaultOptions { fieldLabelModifier = drop 1 }
