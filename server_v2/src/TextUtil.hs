module TextUtil where

import Prelude hiding (length, drop, take)
import Data.Text

extract :: Text -> Text -> Text -> Maybe Text
extract start end str = do
    idx1 <- substringIndex' start str
    let str' = drop (idx1 + length start) str
    idx2 <- substringIndex' end str'
    return $ take idx2 str'

substringIndex :: Text -> Text -> Maybe Int
substringIndex sub = go 0
    where
        subLen = length sub
        go :: Int -> Text -> Maybe Int
        go acc str | subLen > length str    = Nothing
                   | sub == take subLen str = Just acc
                   | otherwise              = go (acc+1) (drop 1 str)

substringIndex' :: Text -> Text -> Maybe Int
substringIndex' sub str | length sub > length str      = Nothing
                        | sub == take (length sub) str = Just 0
                        | otherwise                    = fmap (+1) $ substringIndex sub (drop 1 str)
