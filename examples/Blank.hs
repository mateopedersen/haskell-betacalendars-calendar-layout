module Main (main) where

import BetaCalendars.CalendarLayout.Blank
import BetaCalendars.CalendarLayout.Paper

main :: IO ()
main = do
  print (length (blankCells (blankGrid SixWeeks)))
  print (blankLayout A4 Portrait SixWeeks)
