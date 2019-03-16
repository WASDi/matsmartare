{-# LANGUAGE CPP #-}
module TimeUtil where

#if __GLASGOW_HASKELL__ < 800
import System.Locale (defaultTimeLocale)
#endif

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format

-- Assume timestamp is midnight Swedish time, add 12 hours for timezone fix
formatYMD :: Int -> String
formatYMD = formatTime defaultTimeLocale "%Y-%m-%d" . posixSecondsToUTCTime . fromInteger . toInteger . ((+) (3600 * 12))
