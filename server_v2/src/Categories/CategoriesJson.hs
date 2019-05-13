{-# LANGUAGE DeriveGeneric #-}
module Categories.CategoriesJson where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types

data CategoriesJsonRoot = CategoriesJsonRoot
    { _menuState  :: MenuState
    , _routeState :: RouteState
    } deriving (Show, Generic)

newtype MenuState = MenuState { _menu :: [MenuItem] } deriving (Show, Generic)

data MenuItem = MenuItem
    { m_alias :: String
    , m_label :: String
    } deriving (Show, Generic)

newtype RouteState = RouteState { _routes :: [RouteItem] } deriving (Show, Generic)

data RouteItem = RouteItem
    { r_id       :: String
    , r_alias    :: String
    , r_resource :: String
    } deriving (Show, Generic)

parseDrop1 = defaultOptions { fieldLabelModifier = drop 1 }
parseDrop2 = defaultOptions { fieldLabelModifier = drop 2 }

instance FromJSON CategoriesJsonRoot where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON MenuState where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON MenuItem where
  parseJSON = genericParseJSON parseDrop2

instance FromJSON RouteState where
  parseJSON = genericParseJSON parseDrop1

instance FromJSON RouteItem where
  parseJSON = genericParseJSON parseDrop2
