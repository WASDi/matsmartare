import qualified TimeUtil

import           TestParseCategories

main :: IO ()
main = do
  let datetime = TimeUtil.formatYMD 123456789
  putStrLn $ "TODO add proper assertions using test framework. " ++ datetime
  testParseCategories
-- https://github.com/andybalaam/hunit-example
