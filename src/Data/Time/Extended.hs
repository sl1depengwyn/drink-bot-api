module Data.Time.Extended
  ( module Data.Time,
    nowFormatted,
    mkUTCTime
  )
where

import Data.Fixed (Pico)
import Data.Time

nowFormatted :: IO String
nowFormatted = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime

mkUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) = UTCTime (fromGregorian year mon day) (timeOfDayToTime (TimeOfDay hour min sec))