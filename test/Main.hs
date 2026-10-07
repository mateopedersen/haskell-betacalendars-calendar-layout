module Main (main) where

import BetaCalendars.CalendarLayout.Blank
import BetaCalendars.CalendarLayout.Boundary2027
import BetaCalendars.CalendarLayout.Civil
import BetaCalendars.CalendarLayout.Grid
import BetaCalendars.CalendarLayout.Paper
import BetaCalendars.CalendarLayout.Validation
import BetaCalendars.CalendarLayout.WeekStart
import System.Exit (exitFailure)

main :: IO ()
main = do
  let finite =
        [ (show y ++ "/" ++ show m ++ "/" ++ show start ++ "/" ++ show mode,
            let grid = buildMonthGrid y m start mode IncludeAdjacentDates
                firstCurrent = case [i | (i, cell) <- zip [0 ..] (gridCells grid), cellRelation cell == CurrentMonth] of
                  i : _ -> i
                  [] -> -1
                expectedOffset = offsetFromWeekStart start (firstWeekday y m)
            in null (validateMonthGrid grid)
              && firstCurrent == expectedOffset
              && (mode /= FixedSixWeeks || length (gridCells grid) == 42))
        | y <- [Year n | n <- [1900 .. 2100]]
        , m <- allMonths
        , start <- allWeekStarts
        , mode <- [NaturalRows, FixedSixWeeks]
        ]
      leaps =
        [ ("1900 common", not (isLeapYear (Year 1900)))
        , ("2000 leap", isLeapYear (Year 2000))
        , ("2024 leap", isLeapYear (Year 2024))
        , ("2027 common", not (isLeapYear (Year 2027)))
        , ("2100 common", not (isLeapYear (Year 2100)))
        , ("2400 leap", isLeapYear (Year 2400))
        ]
      papers =
        [ (show paper ++ " " ++ show orientation,
            case layoutMetrics (defaultLayoutSpec paper orientation 6) of
              Right metrics -> unMillimeters (cellWidth metrics) > 0
                && unMillimeters (cellHeight metrics) > 0 && cellArea metrics > 0
              Left _ -> False)
        | paper <- [A4, A5, Letter, Legal]
        , orientation <- [Portrait, Landscape]
        ]
      blankChecks =
        [ ("blank five weeks", length (blankCells (blankGrid FiveWeeks)) == 35)
        , ("blank six weeks", length (blankCells (blankGrid SixWeeks)) == 42)
        , ("blank adjacent dates", all ((== EmptyCell) . cellRelation)
            (take 4 (gridCells (buildMonthGrid (Year 2027) January MondayStart NaturalRows BlankAdjacentCells))))
        ]
      boundaryChecks =
        [ ("Nov 2026 has 30 days", daysInMonth (Year 2026) November == 30)
        , ("Dec 2026 has 31 days", daysInMonth (Year 2026) December == 31)
        , ("Feb 2027 has 28 days", daysInMonth (Year 2027) February == 28)
        , ("year transition", dateYear (nextCivilDate (lastDate (Year 2026) December)) == Year 2027)
        , ("boundary fixture validation", null validateBoundaryFixture && length boundary2027Grids == 4)
        ]
      invalidLayout = (defaultLayoutSpec A4 Portrait 6)
        { layoutMargins = Margins (Millimeters (-1)) (Millimeters 12) (Millimeters 12) (Millimeters 12) }
      overflowLayout = (defaultLayoutSpec A4 Portrait 6) { layoutNotesHeight = Millimeters 1000 }
      invalidChecks =
        [ ("negative margin rejected", case layoutMetrics invalidLayout of Left InvalidMargins -> True; _ -> False)
        , ("reserved-area overflow rejected", case layoutMetrics overflowLayout of Left ReservedAreaOverflow -> True; _ -> False)
        ]
      assertions = leaps ++ papers ++ blankChecks ++ boundaryChecks ++ invalidChecks ++ finite
      failures = filter (not . snd) assertions
  if null failures
    then putStrLn ("PASS: " ++ show (length assertions) ++ " deterministic assertions")
    else do
      mapM_ (putStrLn . ("FAIL: " ++) . fst) failures
      exitFailure
