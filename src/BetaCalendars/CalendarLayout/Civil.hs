-- | Proleptic Gregorian civil dates and month boundaries. No time zone or
-- timestamp semantics are involved.
module BetaCalendars.CalendarLayout.Civil
  ( Year (..)
  , Month (..)
  , allMonths
  , CivilDate
  , civilDate
  , civilDateDay
  , dateYear
  , dateMonth
  , dateDay
  , Weekday (..)
  , weekdayOf
  , isLeapYear
  , daysInMonth
  , firstDate
  , lastDate
  , firstWeekday
  , lastWeekday
  , nextCivilDate
  , previousCivilDate
  ) where

import Data.Time.Calendar (Day, addDays, dayOfWeek, fromGregorian, fromGregorianValid, toGregorian)
import qualified Data.Time.Calendar.WeekDate as W

-- | A Gregorian calendar year. 'Integer' avoids an artificial machine-year limit.
newtype Year = Year
  { unYear :: Integer -- ^ Numeric Gregorian year.
  }
  deriving (Eq, Ord, Show)

-- | The twelve Gregorian months, in calendar order. The constructor order is
-- January through December and is used by 'allMonths'.
data Month = January | February | March | April | May | June
  | July | August | September | October | November | December
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Months in chronological order from January to December.
allMonths :: [Month]
allMonths = [minBound .. maxBound]

-- | A valid Gregorian civil date, represented internally by @time@'s 'Day'.
newtype CivilDate = CivilDate
  { civilDateDay :: Day -- ^ Underlying day value.
  }
  deriving (Eq, Ord, Show)

-- | Construct a date, returning 'Nothing' for an invalid month day.
civilDate :: Year -> Month -> Int -> Maybe CivilDate
civilDate (Year y) m d = CivilDate <$> fromGregorianValid y (monthNumber m) d

-- | Year component of a valid civil date.
dateYear :: CivilDate -> Year
dateYear (CivilDate d) = let (y, _, _) = toGregorian d in Year y

-- | Month component of a valid civil date.
dateMonth :: CivilDate -> Month
dateMonth (CivilDate d) = let (_, m, _) = toGregorian d in monthFromNumber m

-- | Day-of-month component, in the range 1–31.
dateDay :: CivilDate -> Int
dateDay (CivilDate d) = let (_, _, day) = toGregorian d in day

-- | Monday-based weekday names, ordered Monday through Sunday.
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Weekday of a valid civil date.
weekdayOf :: CivilDate -> Weekday
weekdayOf (CivilDate d) = case dayOfWeek d of
  W.Monday -> Monday
  W.Tuesday -> Tuesday
  W.Wednesday -> Wednesday
  W.Thursday -> Thursday
  W.Friday -> Friday
  W.Saturday -> Saturday
  W.Sunday -> Sunday

-- | Gregorian leap-year rule, including century exceptions.
isLeapYear :: Year -> Bool
isLeapYear (Year y) = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

-- | Number of days in a month.
daysInMonth :: Year -> Month -> Int
daysInMonth y m = case m of
  January -> 31
  February -> if isLeapYear y then 29 else 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

-- | First civil date in a month.
firstDate :: Year -> Month -> CivilDate
firstDate y m = CivilDate (fromGregorian (unYear y) (monthNumber m) 1)

-- | Last civil date in a month.
lastDate :: Year -> Month -> CivilDate
lastDate y m = CivilDate (fromGregorian (unYear y) (monthNumber m) (daysInMonth y m))

-- | Weekday of the first date in a month.
firstWeekday :: Year -> Month -> Weekday
firstWeekday year month = weekdayOf (firstDate year month)

-- | Weekday of the last date in a month.
lastWeekday :: Year -> Month -> Weekday
lastWeekday year month = weekdayOf (lastDate year month)

-- | Following civil day.
nextCivilDate :: CivilDate -> CivilDate
nextCivilDate (CivilDate d) = CivilDate (addDays 1 d)

-- | Previous civil day.
previousCivilDate :: CivilDate -> CivilDate
previousCivilDate (CivilDate d) = CivilDate (addDays (-1) d)

monthNumber :: Month -> Int
monthNumber = (+ 1) . fromEnum

monthFromNumber :: Int -> Month
monthFromNumber n = case n of
  1 -> January
  2 -> February
  3 -> March
  4 -> April
  5 -> May
  6 -> June
  7 -> July
  8 -> August
  9 -> September
  10 -> October
  11 -> November
  _ -> December
