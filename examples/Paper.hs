module Main (main) where

import BetaCalendars.CalendarLayout.Paper

main :: IO ()
main = print (layoutMetrics (defaultLayoutSpec A4 Portrait 6))
