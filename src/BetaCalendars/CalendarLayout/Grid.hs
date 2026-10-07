-- | Pure month-grid construction and descriptive topology values.
module BetaCalendars.CalendarLayout.Grid
  ( GridMode (..)
  , AdjacentPolicy (..)
  , CellRelation (..)
  , GridCell (..)
  , MonthGrid (..)
  , MonthTopology (..)
  , TopologySignature (..)
  , buildMonthGrid
  , monthTopology
  , naturalRowCount
  , leadingCellCount
  , trailingCellCount
  , gridOccupancy
  , topologySignature
  ) where

import BetaCalendars.CalendarLayout.Civil
import BetaCalendars.CalendarLayout.WeekStart

-- | Use the month’s natural shape or a fixed 6 × 7 page grid.
data GridMode = NaturalRows | FixedSixWeeks
  deriving (Eq, Ord, Show, Read)

-- | Whether out-of-month grid positions show dates or remain undated blanks.
data AdjacentPolicy = IncludeAdjacentDates | BlankAdjacentCells
  deriving (Eq, Ord, Show, Read)

-- | Semantic relation of a cell to the requested month.
data CellRelation = PreviousMonth | CurrentMonth | NextMonth | EmptyCell
  deriving (Eq, Ord, Show, Read)

-- | One month-grid position and its date, if one is shown.
data GridCell = GridCell
  { cellDate :: Maybe CivilDate -- ^ No date for an intentionally blank cell.
  , cellRelation :: CellRelation -- ^ Position’s semantic relation.
  } deriving (Eq, Show)

-- | A row-major rectangular month grid with its construction parameters.
data MonthGrid = MonthGrid
  { gridYear :: Year -- ^ Requested civil year.
  , gridMonth :: Month -- ^ Requested month.
  , gridWeekStart :: WeekStart -- ^ First weekday in each row.
  , gridMode :: GridMode -- ^ Natural or fixed six-week shape.
  , gridAdjacentPolicy :: AdjacentPolicy -- ^ Visibility of adjacent dates.
  , gridRows :: Int -- ^ Number of rows in the result.
  , gridCells :: [GridCell] -- ^ Cells in row-major order.
  } deriving (Eq, Show)

-- | Week-origin-specific structure of a Gregorian month.
data MonthTopology = MonthTopology
  { topologyDays :: Int -- ^ Number of dates in the month.
  , topologyFirstWeekday :: Weekday -- ^ Weekday of its first date.
  , topologyLastWeekday :: Weekday -- ^ Weekday of its last date.
  , topologyLeadingCells :: Int -- ^ Cells before the first date.
  , topologyTrailingCells :: Int -- ^ Cells after the last date in a natural grid.
  , topologyNaturalRows :: Int -- ^ Natural row count, from four through six.
  , topologyFixedCells :: Int -- ^ Fixed grid size, always 42.
  } deriving (Eq, Show)

-- | Library-specific summary; not a standardized calendar metric.
data TopologySignature = TopologySignature
  { signatureDays :: Int -- ^ Current-month cell count.
  , signatureStartOffset :: Int -- ^ Leading-cell count.
  , signatureNaturalRows :: Int -- ^ Natural row count.
  , signatureTrailingCapacity :: Int -- ^ Trailing cells in the natural grid.
  } deriving (Eq, Ord, Show, Read)

-- | Build a deterministic month grid for any week origin and adjacent policy.
buildMonthGrid :: Year -> Month -> WeekStart -> GridMode -> AdjacentPolicy -> MonthGrid
buildMonthGrid year month weekStart mode policy = MonthGrid
  { gridYear = year
  , gridMonth = month
  , gridWeekStart = weekStart
  , gridMode = mode
  , gridAdjacentPolicy = policy
  , gridRows = rows
  , gridCells = map makeCell offsets
  }
  where
    leading = leadingCellCount year month weekStart
    count = daysInMonth year month
    naturalCells = leading + count
    rows = case mode of
      NaturalRows -> (naturalCells + 6) `div` 7
      FixedSixWeeks -> 6
    size = rows * 7
    first = firstDate year month
    offsets = [-leading .. size - leading - 1]
    makeCell n =
      let date = shiftDate first n
          relation = relationTo year month date
          visibleDate = case (policy, relation) of
            (BlankAdjacentCells, CurrentMonth) -> Just date
            (BlankAdjacentCells, _) -> Nothing
            (IncludeAdjacentDates, _) -> Just date
      in GridCell visibleDate (if visibleDate == Nothing then EmptyCell else relation)

-- | Describe the month’s topology for one configured week origin.
monthTopology :: Year -> Month -> WeekStart -> MonthTopology
monthTopology year month start = MonthTopology
  { topologyDays = daysInMonth year month
  , topologyFirstWeekday = firstWeekday year month
  , topologyLastWeekday = lastWeekday year month
  , topologyLeadingCells = leading
  , topologyTrailingCells = naturalRows * 7 - leading - daysInMonth year month
  , topologyNaturalRows = naturalRows
  , topologyFixedCells = 42
  }
  where
    leading = leadingCellCount year month start
    naturalRows = naturalRowCount year month start

-- | Natural row count, which is always four, five, or six.
naturalRowCount :: Year -> Month -> WeekStart -> Int
naturalRowCount year month start = (leadingCellCount year month start + daysInMonth year month + 6) `div` 7

-- | Number of cells before the first day under the selected week origin.
leadingCellCount :: Year -> Month -> WeekStart -> Int
leadingCellCount year month start = offsetFromWeekStart start (firstWeekday year month)

-- | Number of cells after the last day in a natural grid.
trailingCellCount :: Year -> Month -> WeekStart -> Int
trailingCellCount year month start = naturalRowCount year month start * 7 - leadingCellCount year month start - daysInMonth year month

-- | Fraction of the grid occupied by current-month dates; empty grids yield 0.
gridOccupancy :: MonthGrid -> Rational
gridOccupancy grid
  | null (gridCells grid) = 0
  | otherwise = fromIntegral current / fromIntegral (length (gridCells grid))
  where current = length (filter ((== CurrentMonth) . cellRelation) (gridCells grid))

-- | Convert a topology to this library’s descriptive signature value.
topologySignature :: MonthTopology -> TopologySignature
topologySignature t = TopologySignature
  (topologyDays t) (topologyLeadingCells t) (topologyNaturalRows t) (topologyTrailingCells t)

shiftDate :: CivilDate -> Int -> CivilDate
shiftDate date n = case civilDate (dateYear date) (dateMonth date) (dateDay date + n) of
  Just result -> result
  Nothing -> shiftAcrossMonth date n

-- Calendar arithmetic through adjacent month boundaries without partial functions.
shiftAcrossMonth :: CivilDate -> Int -> CivilDate
shiftAcrossMonth date n
  | n < 0 = shiftAcrossMonth (previousCivilDate date) (n + 1)
  | n > 0 = shiftAcrossMonth (nextCivilDate date) (n - 1)
  | otherwise = date

relationTo :: Year -> Month -> CivilDate -> CellRelation
relationTo y m date
  | dateYear date == y && dateMonth date == m = CurrentMonth
  | date < firstDate y m = PreviousMonth
  | otherwise = NextMonth
