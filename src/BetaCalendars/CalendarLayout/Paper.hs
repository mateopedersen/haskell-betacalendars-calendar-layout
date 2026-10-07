-- | Physical paper dimensions and deterministic printable-grid geometry.
-- Printable comparisons: [blank](https://www.betacalendars.com/blank-calendar),
-- [January](https://www.betacalendars.com/january-calendar.html),
-- [July](https://www.betacalendars.com/july-calendar.html), and
-- [December](https://www.betacalendars.com/december-calendar.html).
module BetaCalendars.CalendarLayout.Paper
  ( Millimeters (..)
  , PaperSize (..)
  , Orientation (..)
  , Margins (..)
  , LayoutSpec (..)
  , LayoutError (..)
  , LayoutMetrics (..)
  , paperDimensions
  , layoutMetrics
  , defaultLayoutSpec
  ) where

-- | Exact physical distance in millimetres, represented as a rational number.
newtype Millimeters = Millimeters
  { unMillimeters :: Rational -- ^ Exact distance; this is not a CSS pixel value.
  }
  deriving (Eq, Ord, Show, Read)

-- | Standard page sizes or a custom width and height.
data PaperSize
  = A4 -- ^ ISO 216 A4, 210 × 297 mm.
  | A5 -- ^ ISO 216 A5, 148 × 210 mm.
  | Letter -- ^ US Letter, 215.9 × 279.4 mm.
  | Legal -- ^ US Legal, 215.9 × 355.6 mm.
  | CustomPaper Millimeters Millimeters -- ^ Custom width followed by height.
  deriving (Eq, Ord, Show, Read)

-- | Page orientation; landscape swaps the width and height.
data Orientation = Portrait | Landscape
  deriving (Eq, Ord, Show, Read)

-- | Distances between printable content and the four page edges.
data Margins = Margins
  { marginTop :: Millimeters -- ^ Top edge margin.
  , marginRight :: Millimeters -- ^ Right edge margin.
  , marginBottom :: Millimeters -- ^ Bottom edge margin.
  , marginLeft :: Millimeters -- ^ Left edge margin.
  } deriving (Eq, Ord, Show, Read)

-- | Inputs defining a rectangular grid and reserved title/header/notes bands.
data LayoutSpec = LayoutSpec
  { layoutPaper :: PaperSize -- ^ Physical sheet.
  , layoutOrientation :: Orientation -- ^ Portrait or landscape.
  , layoutMargins :: Margins -- ^ Edge clearances.
  , layoutTitleHeight :: Millimeters -- ^ Height reserved for a title.
  , layoutWeekdayHeaderHeight :: Millimeters -- ^ Height reserved for weekday labels.
  , layoutNotesHeight :: Millimeters -- ^ Height reserved for notes.
  , layoutRows :: Int -- ^ Positive number of grid rows.
  , layoutColumns :: Int -- ^ Positive number of grid columns.
  } deriving (Eq, Show, Read)

-- | Reasons a proposed paper layout cannot produce positive cell geometry.
data LayoutError
  = InvalidPaperDimensions -- ^ Page width or height is not positive.
  | InvalidMargins -- ^ At least one edge margin is negative.
  | InvalidGridDimensions -- ^ Row or column count is not positive.
  | InvalidReservedArea -- ^ A reserved band has negative height.
  | ReservedAreaOverflow -- ^ Reserved bands leave no room for the grid.
  | NonPositivePrintableArea -- ^ Margins leave no positive printable rectangle.
  | NonPositiveCellGeometry -- ^ Computed cell width or height is not positive.
  deriving (Eq, Ord, Show, Read)

-- | Derived page, printable region, grid, and cell measurements.
data LayoutMetrics = LayoutMetrics
  { pageWidth :: Millimeters -- ^ Oriented page width.
  , pageHeight :: Millimeters -- ^ Oriented page height.
  , printableWidth :: Millimeters -- ^ Width after left/right margins.
  , printableHeight :: Millimeters -- ^ Height after top/bottom margins.
  , gridWidth :: Millimeters -- ^ Width occupied by the grid.
  , gridHeight :: Millimeters -- ^ Height occupied by the grid.
  , cellWidth :: Millimeters -- ^ Width of each equal grid column.
  , cellHeight :: Millimeters -- ^ Height of each equal grid row.
  , cellArea :: Rational -- ^ Area of one cell in square millimetres.
  , writingArea :: Rational -- ^ Grid area in square millimetres.
  } deriving (Eq, Show, Read)

-- | Exact dimensions for standard and custom paper sizes, before orientation.
paperDimensions :: PaperSize -> (Millimeters, Millimeters)
paperDimensions size = case size of
  A4 -> (mm 210, mm 297)
  A5 -> (mm 148, mm 210)
  Letter -> (mmFraction 2159 10, mmFraction 2794 10)
  Legal -> (mmFraction 2159 10, mmFraction 3556 10)
  CustomPaper w h -> (w, h)

-- | A useful default with 12 mm margins, 18 mm title, 8 mm weekday band,
-- and 20 mm notes band, using seven columns.
defaultLayoutSpec :: PaperSize -> Orientation -> Int -> LayoutSpec
defaultLayoutSpec paper orientation rows = LayoutSpec
  { layoutPaper = paper
  , layoutOrientation = orientation
  , layoutMargins = Margins (mm 12) (mm 12) (mm 12) (mm 12)
  , layoutTitleHeight = mm 18
  , layoutWeekdayHeaderHeight = mm 8
  , layoutNotesHeight = mm 20
  , layoutRows = rows
  , layoutColumns = 7
  }

-- | Validate a layout and compute exact dimensions, or return a domain error.
layoutMetrics :: LayoutSpec -> Either LayoutError LayoutMetrics
layoutMetrics spec
  | width <= 0 || height <= 0 = Left InvalidPaperDimensions
  | any (< 0) margins = Left InvalidMargins
  | layoutRows spec <= 0 || layoutColumns spec <= 0 = Left InvalidGridDimensions
  | any (< 0) reserved = Left InvalidReservedArea
  | printableW <= 0 || printableH <= 0 = Left NonPositivePrintableArea
  | gridH <= 0 = Left ReservedAreaOverflow
  | cellW <= 0 || cellH <= 0 = Left NonPositiveCellGeometry
  | otherwise = Right LayoutMetrics
      { pageWidth = Millimeters pageW
      , pageHeight = Millimeters pageH
      , printableWidth = Millimeters printableW
      , printableHeight = Millimeters printableH
      , gridWidth = Millimeters printableW
      , gridHeight = Millimeters gridH
      , cellWidth = Millimeters cellW
      , cellHeight = Millimeters cellH
      , cellArea = cellW * cellH
      , writingArea = printableW * gridH
      }
  where
    (paperW, paperH) = paperDimensions (layoutPaper spec)
    (width, height) = case layoutOrientation spec of
      Portrait -> (unMillimeters paperW, unMillimeters paperH)
      Landscape -> (unMillimeters paperH, unMillimeters paperW)
    (top, right, bottom, left) = toTuple (layoutMargins spec)
    pageW = width
    pageH = height
    margins = [top, right, bottom, left]
    printableW = width - right - left
    printableH = height - top - bottom
    reserved = [unMillimeters (layoutTitleHeight spec), unMillimeters (layoutWeekdayHeaderHeight spec), unMillimeters (layoutNotesHeight spec)]
    gridH = printableH - sum reserved
    cellW = printableW / fromIntegral (layoutColumns spec)
    cellH = gridH / fromIntegral (layoutRows spec)

mm :: Integer -> Millimeters
mm = Millimeters . fromInteger

mmFraction :: Integer -> Integer -> Millimeters
mmFraction n d = Millimeters (fromInteger n / fromInteger d)

toTuple :: Margins -> (Rational, Rational, Rational, Rational)
toTuple m = (unMillimeters (marginTop m), unMillimeters (marginRight m), unMillimeters (marginBottom m), unMillimeters (marginLeft m))
