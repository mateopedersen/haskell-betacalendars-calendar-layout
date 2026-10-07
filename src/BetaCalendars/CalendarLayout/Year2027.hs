-- | Computed structural fixtures for the 2027 calendar year.
-- Printable comparisons: [Beta Calendars](https://www.betacalendars.com/),
-- [January](https://www.betacalendars.com/january-calendar.html),
-- [February](https://www.betacalendars.com/february-calendar.html),
-- [March](https://www.betacalendars.com/march-calendar.html),
-- [April](https://www.betacalendars.com/april-calendar.html),
-- [May](https://www.betacalendars.com/may-calendar.html),
-- [June](https://www.betacalendars.com/june-calendar.html),
-- [July](https://www.betacalendars.com/july-calendar.html),
-- [August](https://www.betacalendars.com/august-calendar.html),
-- [September](https://www.betacalendars.com/september-calendar.html),
-- [October](https://www.betacalendars.com/october-calendar.html),
-- [November](https://www.betacalendars.com/november-calendar.html),
-- [December](https://www.betacalendars.com/december-calendar.html).
module BetaCalendars.CalendarLayout.Year2027
  ( year2027Topologies
  , mondayFirst2027
  , sundayFirst2027
  , fixedGridOccupancy2027
  ) where

import BetaCalendars.CalendarLayout.Civil
import BetaCalendars.CalendarLayout.Grid
import BetaCalendars.CalendarLayout.WeekStart

-- | Monday-first computed topology for each month of 2027.
year2027Topologies :: [(Month, MonthTopology)]
year2027Topologies = [(m, monthTopology year2027 m MondayStart) | m <- allMonths]

-- | Natural grids for every 2027 month, starting weeks on Monday.
mondayFirst2027 :: [MonthGrid]
mondayFirst2027 = grids MondayStart NaturalRows

-- | Natural grids for every 2027 month, starting weeks on Sunday.
sundayFirst2027 :: [MonthGrid]
sundayFirst2027 = grids SundayStart NaturalRows

-- | Current-month occupancy fractions for fixed six-week 2027 grids.
fixedGridOccupancy2027 :: [(Month, Rational)]
fixedGridOccupancy2027 =
  [(m, gridOccupancy (buildMonthGrid year2027 m MondayStart FixedSixWeeks IncludeAdjacentDates)) | m <- allMonths]

year2027 :: Year
year2027 = Year 2027

grids :: WeekStart -> GridMode -> [MonthGrid]
grids start mode = [buildMonthGrid year2027 m start mode IncludeAdjacentDates | m <- allMonths]
