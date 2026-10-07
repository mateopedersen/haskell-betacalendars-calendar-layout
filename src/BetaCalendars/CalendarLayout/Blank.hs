-- | Undated planning grids; blank cells deliberately carry no civil dates.
-- See the [printable blank calendar](https://www.betacalendars.com/blank-calendar).
module BetaCalendars.CalendarLayout.Blank
  ( BlankRows (..)
  , BlankCell (..)
  , BlankGrid (..)
  , blankGrid
  , blankLayout
  , blankWritingArea
  , compareBlankLayouts
  ) where

import BetaCalendars.CalendarLayout.Paper

-- | Supported undated planner heights.
data BlankRows = FiveWeeks | SixWeeks
  deriving (Eq, Ord, Show, Read)

-- | A blank position with zero-based row and column coordinates.
data BlankCell = BlankCell
  { blankRow :: Int -- ^ Row, starting at zero.
  , blankColumn :: Int -- ^ Column, starting at zero.
  } deriving (Eq, Ord, Show, Read)

-- | A date-free rectangular planner grid.
data BlankGrid = BlankGrid
  { blankRows :: BlankRows -- ^ Five or six week rows.
  , blankColumns :: Int -- ^ Seven weekday columns.
  , blankCells :: [BlankCell] -- ^ Cells in row-major order.
  } deriving (Eq, Show)

-- | Create an undated grid with seven columns and the requested row count.
blankGrid :: BlankRows -> BlankGrid
blankGrid rows = BlankGrid rows 7 [BlankCell r c | r <- [0 .. rowCount - 1], c <- [0 .. 6]]
  where rowCount = case rows of FiveWeeks -> 5; SixWeeks -> 6

-- | Compute paper metrics for an undated grid.
blankLayout :: PaperSize -> Orientation -> BlankRows -> Either LayoutError LayoutMetrics
blankLayout paper orientation rows = layoutMetrics (defaultLayoutSpec paper orientation rowCount)
  where rowCount = case rows of FiveWeeks -> 5; SixWeeks -> 6

-- | Usable writing area in square millimetres.
blankWritingArea :: LayoutMetrics -> Rational
blankWritingArea = writingArea

-- | Signed ratio of usable writing areas; zero is returned for a zero baseline.
compareBlankLayouts :: LayoutMetrics -> LayoutMetrics -> Rational
compareBlankLayouts a b
  | writingArea b == 0 = 0
  | otherwise = writingArea a / writingArea b
