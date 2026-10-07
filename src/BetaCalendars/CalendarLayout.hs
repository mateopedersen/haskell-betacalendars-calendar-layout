-- | High-level API for deterministic civil calendar grids and print geometry.
-- The project homepage is <https://www.betacalendars.com/>.
module BetaCalendars.CalendarLayout
  ( Year (..)
  , Month (..)
  , allMonths
  , CivilDate
  , civilDate
  , dateYear
  , dateMonth
  , dateDay
  , Weekday (..)
  , WeekStart (..)
  , allWeekStarts
  , GridMode (..)
  , AdjacentPolicy (..)
  , CellRelation (..)
  , GridCell (..)
  , MonthGrid (..)
  , MonthTopology (..)
  , buildMonthGrid
  , monthTopology
  , Millimeters (..)
  , PaperSize (..)
  , Orientation (..)
  , Margins (..)
  , LayoutSpec (..)
  , LayoutError (..)
  , LayoutMetrics (..)
  , layoutMetrics
  ) where

import BetaCalendars.CalendarLayout.Civil
import BetaCalendars.CalendarLayout.Grid
import BetaCalendars.CalendarLayout.Paper
import BetaCalendars.CalendarLayout.WeekStart
