-- | Week-origin arithmetic for all seven possible first weekdays.
module BetaCalendars.CalendarLayout.WeekStart
  ( WeekStart (..)
  , allWeekStarts
  , weekdayIndex
  , relativeWeekdayIndex
  , offsetFromWeekStart
  , rotateWeekOrigin
  ) where

import BetaCalendars.CalendarLayout.Civil (Weekday (..))

-- | Configurable first weekday for a displayed week.
data WeekStart = MondayStart | TuesdayStart | WednesdayStart | ThursdayStart
  | FridayStart | SaturdayStart | SundayStart
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | All seven week origins in Monday-to-Sunday order.
allWeekStarts :: [WeekStart]
allWeekStarts = [minBound .. maxBound]

-- | Monday is 0 and Sunday is 6.
-- | Monday-based zero-index of a weekday.
weekdayIndex :: Weekday -> Int
weekdayIndex = fromEnum

-- | Index of a weekday in a week whose first day is the given origin.
-- | Zero-index of a weekday relative to a selected week origin.
relativeWeekdayIndex :: WeekStart -> Weekday -> Int
relativeWeekdayIndex origin weekday = (weekdayIndex weekday - startIndex origin) `mod` 7

-- | Number of leading cells before the first day of a month.
-- | Leading-cell count for a date with the given weekday.
offsetFromWeekStart :: WeekStart -> Weekday -> Int
offsetFromWeekStart = relativeWeekdayIndex

-- | Rotate weekdays so that the configured origin appears first.
-- | Monday-to-Sunday weekdays rotated to start at the selected origin.
rotateWeekOrigin :: WeekStart -> [Weekday]
rotateWeekOrigin origin = take 7 (drop start (weekdays ++ weekdays))
  where
    weekdays = [Monday .. Sunday]
    start = startIndex origin

startIndex :: WeekStart -> Int
startIndex = fromEnum
