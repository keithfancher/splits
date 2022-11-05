module Parse
  ( parse,
    parseLine,
    CSV,
    CSVLine,
    CSVRaw,
    DateFormat (..),
    DateParseConf (..),
    ParseConf (..),
  )
where

import qualified Data.Text as T
import Error (Error (..), ErrorType (..), mkError)
import Expense (Date (..), Expense (..))
import qualified Text.CSV as C
import Text.Read (readMaybe)

data ParseConf = ParseConf
  { -- Character(s) that separate the columns in the CSV:
    colSep :: T.Text,
    -- Zero-indexed column number of the date of expense
    dateColNum :: Int,
    -- Zero-indexed column number of the amount of expense
    amountColNum :: Int,
    -- Zero-indexed row number of the beginning of the data (i.e. if there's a
    -- header row at `0`, this value might be `1`)
    dataStartRow :: Int,
    -- Conf needed to correctly parse the dates fields in the CSV:
    dateConf :: DateParseConf
  }
  deriving (Eq, Show)

-- Knowing the format (essentially, order of the fields) and the separator, we
-- can parse the given dates.
data DateParseConf = DateParseConf
  { dateFormat :: DateFormat,
    dateSep :: T.Text
  }
  deriving (Eq, Show)

-- As far as I can tell, this sums up pretty much every valid date format, or
-- at least ones likely to be used in a CSV of expenses. Note that this
-- specifies the ORDER of the fields only. We also need to know the separator
-- (e.g. '/' or '-' or whatever). See the full `DateParseConf` type above.
data DateFormat
  = YMD -- y/m/d, aka ISO-8601 (e.g. 2022-10-31)
  | YDM -- y/d/m, aka ??? (e.g. 2022-31-10)
  | MDY -- m/d/y, aka Amercan-style (e.g. 10-31-2022)
  | DMY -- d/m/y, aka Euro-style (e.g. 31-10-2022)
  deriving (Eq, Show, Read)

-- Unparsed, just the raw text
type CSVRaw = T.Text

-- Parsed, a collection of rows/lines/records/whatever
type CSV = [CSVLine]

-- A line is just a collection of fields
type CSVLine = [CSVField]

-- ...which is itself just text
type CSVField = T.Text

-- Given a CSV (as Text), parse it out into a list of `Expense` objects. We'll
-- need some config data to know exactly how to parse out the data we need.
parse :: ParseConf -> CSVRaw -> Either Error [Expense]
parse _ "" = Right [] -- this is fine
parse conf expensesCsv = do
  -- Note call to `strip` -- the `csv` library doesn't like newlines at the end
  -- of an input file, and it generates an extra (invalid) line which then
  -- fails to parse. (The RFC says it's fine tho'!) Anyway, a workaround:
  let csvString = T.unpack $ T.strip expensesCsv
  parseResults <- mapCsvResults $ C.parseCSV "" csvString
  let linesWithoutHeader = dropHeader parseResults
  mapM parseWithConf linesWithoutHeader
  where
    parseWithConf = parseLine conf -- partial application magic!
    dropHeader = drop (dataStartRow conf) -- note that this is valid even if there is no header

-- Map the `csv` library's types into our own.
mapCsvResults :: Show a => Either a C.CSV -> Either Error CSV
mapCsvResults (Left e) = Left $ mkError ParseError $ T.pack $ show e
mapCsvResults (Right r) = Right $ mapCsv r
  where
    mapCsv = map mapRecord
    mapRecord = map mapField
    mapField = T.pack

--  One row from the CSV. Parse out a single `Expense` object.
parseLine :: ParseConf -> CSVLine -> Either Error Expense
parseLine conf csvFields = Expense <$> expDate <*> expAmount
  where
    expDate = (csvFields `nth` dateColNum conf) >>= parseDate (dateConf conf)
    amountText = csvFields `nth` amountColNum conf
    expAmount = amountText >>= readDouble

-- Attempt to parse out a single `Date`, given some config data and Text.
parseDate :: DateParseConf -> T.Text -> Either Error Date
parseDate conf dateText = toDate (dateFormat conf)
  where
    splitText = T.splitOn (dateSep conf) dateText
    toInt index = splitText `nth` index >>= readInt
    -- Grab fields from correct indices based on date format (our own internal
    -- `Date` object is essentially Y M D):
    toDate YMD = Date <$> toInt 0 <*> toInt 1 <*> toInt 2
    toDate YDM = Date <$> toInt 0 <*> toInt 2 <*> toInt 1
    toDate MDY = Date <$> toInt 2 <*> toInt 0 <*> toInt 1
    toDate DMY = Date <$> toInt 2 <*> toInt 1 <*> toInt 0

readDouble :: T.Text -> Either Error Double
readDouble t = case readMaybe (T.unpack t) of
  Just d -> Right d
  Nothing -> Left $ mkError ParseError ("Error parsing double value: " <> t)

readInt :: T.Text -> Either Error Int
readInt t = case readMaybe (T.unpack t) of
  Just i -> Right i
  Nothing -> Left $ mkError ParseError ("Error parsing integer value: " <> t)

-- A (hopefully?!) safe version of `!!`. Won't die with invalid index, has a
-- maybe-useful error message. Limiting to instances of `Show` for better error
-- messaging.
nth :: Show a => [a] -> Int -> Either Error a
nth xs i = case drop i xs of
  x : _ -> Right x
  [] -> Left $ mkError InvalidInput message
    where
      message = "Out of bounds index: `" <> text i <> "` in array: " <> text xs

-- Convert any Show instance to Text
text :: Show a => a -> T.Text
text = T.pack . show
