module Main (main) where

import BetaCalendars.CalendarLayout
import BetaCalendars.CalendarLayout.Year2027 (year2027Topologies)

main :: IO ()
main = do
  print (monthTopology (Year 2027) January MondayStart)
  print (length (gridCells (buildMonthGrid (Year 2027) January SundayStart FixedSixWeeks IncludeAdjacentDates)))
  print (length year2027Topologies)
