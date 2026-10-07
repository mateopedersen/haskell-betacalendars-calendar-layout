-- | Computed regression fixtures spanning the 2026–2027 year boundary.
-- Printable comparisons: [November](https://www.betacalendars.com/november-calendar.html),
-- [December](https://www.betacalendars.com/december-calendar.html),
-- [January](https://www.betacalendars.com/january-calendar.html), and
-- [February](https://www.betacalendars.com/february-calendar.html).
module BetaCalendars.CalendarLayout.Boundary2027
  ( boundary2027Months
  , boundary2027Grids
  ) where

import BetaCalendars.CalendarLayout.Civil
import BetaCalendars.CalendarLayout.Grid
import BetaCalendars.CalendarLayout.WeekStart

-- | November and December 2026 followed by January and February 2027.
boundary2027Months :: [(Year, Month)]
boundary2027Months =
  [ (Year 2026, November)
  , (Year 2026, December)
  , (Year 2027, January)
  , (Year 2027, February)
  ]

-- | Monday-first natural month grids for 'boundary2027Months'.
boundary2027Grids :: [MonthGrid]
boundary2027Grids =
  [buildMonthGrid y m MondayStart NaturalRows IncludeAdjacentDates | (y, m) <- boundary2027Months]
