-- | Diagnostic validators for grids, finite date ranges and paper geometry.
module BetaCalendars.CalendarLayout.Validation
  ( ValidationIssue (..)
  , validateMonthGrid
  , validateDateCompleteness
  , validateFixedGrid
  , validatePaperLayout
  , validateYear
  , validateBoundaryFixture
  , validateFiniteRange
  ) where

import BetaCalendars.CalendarLayout.Boundary2027 (boundary2027Grids)
import BetaCalendars.CalendarLayout.Civil
import BetaCalendars.CalendarLayout.Grid
import BetaCalendars.CalendarLayout.Paper
import BetaCalendars.CalendarLayout.WeekStart

-- | Structured failure details returned by the reusable validators.
data ValidationIssue
  = WrongCellCount Int Int
  | WrongRowCount Int Int
  | MissingCurrentDates [Int]
  | DuplicateCurrentDates [Int]
  | InvalidNaturalRows Int
  | WrongBoundaryFixtureLength Int Int
  deriving (Eq, Show)

-- | Check row count, cell count, current-date completeness, and mode invariants.
validateMonthGrid :: MonthGrid -> [ValidationIssue]
validateMonthGrid grid = validateDateCompleteness grid ++ rowIssues ++ fixedIssues
  where
    expectedRows = case gridMode grid of
      NaturalRows -> naturalRowCount (gridYear grid) (gridMonth grid) (gridWeekStart grid)
      FixedSixWeeks -> 6
    expectedCells = expectedRows * 7
    rowIssues =
      [WrongRowCount expectedRows (gridRows grid) | gridRows grid /= expectedRows]
      ++ [WrongCellCount expectedCells (length (gridCells grid)) | length (gridCells grid) /= expectedCells]
      ++ [InvalidNaturalRows (gridRows grid) | gridMode grid == NaturalRows && not (gridRows grid >= 4 && gridRows grid <= 6)]
    fixedIssues = [WrongCellCount 42 (length (gridCells grid)) | gridMode grid == FixedSixWeeks && length (gridCells grid) /= 42]

-- | Verify every current-month date occurs exactly once.
validateDateCompleteness :: MonthGrid -> [ValidationIssue]
validateDateCompleteness grid = missingIssue ++ duplicateIssue
  where
    observed = [dateDay d | cell <- gridCells grid, cellRelation cell == CurrentMonth, Just d <- [cellDate cell]]
    expected = [1 .. daysInMonth (gridYear grid) (gridMonth grid)]
    missing = filter (`notElem` observed) expected
    duplicates = [d | d <- expected, length (filter (== d) observed) > 1]
    missingIssue = [MissingCurrentDates missing | not (null missing)]
    duplicateIssue = [DuplicateCurrentDates duplicates | not (null duplicates)]

-- | Check the 42-cell invariant when the supplied grid is fixed mode.
validateFixedGrid :: MonthGrid -> [ValidationIssue]
validateFixedGrid grid
  | gridMode grid /= FixedSixWeeks = []
  | length (gridCells grid) == 42 && gridRows grid == 6 = validateDateCompleteness grid
  | otherwise = [WrongCellCount 42 (length (gridCells grid))]

-- | Validate paper geometry and return computed metrics on success.
validatePaperLayout :: LayoutSpec -> Either LayoutError LayoutMetrics
validatePaperLayout = layoutMetrics

-- | Validate all months, week origins, and both grid modes for one year.
validateYear :: Year -> [ValidationIssue]
validateYear y = concat
  [validateMonthGrid (buildMonthGrid y m start mode IncludeAdjacentDates)
  | m <- allMonths, start <- allWeekStarts, mode <- [NaturalRows, FixedSixWeeks]]

-- | Validate the four-month fixture spanning the 2026–2027 year boundary.
validateBoundaryFixture :: [ValidationIssue]
validateBoundaryFixture
  | length boundary2027Grids == 4 = concatMap validateMonthGrid boundary2027Grids
  | otherwise = [WrongBoundaryFixtureLength 4 (length boundary2027Grids)]

-- | Validate every month from 1900 through 2100 for all seven origins and modes.
validateFiniteRange :: [ValidationIssue]
validateFiniteRange = concatMap validateYear [Year y | y <- [1900 .. 2100]]
