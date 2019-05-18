module TextUtil where

import Prelude hiding (length, drop, take)
import Data.Text
import Data.List (elemIndex)

extract :: Text -> Text -> Text -> Maybe Text
extract start end str = do
    idx1 <- substringIndex start str
    let str' = drop (idx1 + length start) str
    idx2 <- substringIndex end str'
    return $ take idx2 str'

substringIndex :: Text -> Text -> Maybe Int
substringIndex sub str = let subLen = length sub
                             parts  = Prelude.map (take subLen) $ tails str
                         in  elemIndex sub parts
