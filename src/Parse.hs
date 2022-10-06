module Parse
  ( parse,
    parseLine,
    DateFormat (..),
    DateParseConf (..),
    ParseConf (..),
  )
where

import qualified Data.Text as T
import Error (Error (..), ErrorType (..), mkError)
import Expense (Date (..), Expense (..))
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

-- Given a CSV (as Text), parse it out into a list of `Expense` objects. We'll
-- need some config data to know exactly how to parse out the data we need.
parse :: ParseConf -> T.Text -> Either Error [Expense]
parse conf expensesCsv = mapM parseWithConf linesWithoutHeader
  where
    parseWithConf = parseLine conf -- partial application magic!
    linesWithoutHeader = drop (dataStartRow conf) (T.lines expensesCsv)

--  One row from the CSV. Parse out a single `Expense` object.
parseLine :: ParseConf -> T.Text -> Either Error Expense
parseLine conf csvLine = Expense <$> date <*> amount
  where
    splitText = T.splitOn (colSep conf) csvLine
    date = (splitText `nth` dateColNum conf) >>= parseDate (dateConf conf)
    amountText = splitText `nth` amountColNum conf
    amount = amountText >>= readDouble

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
  [] -> Left $ mkError InvalidInput msg
    where
      msg = "Out of bounds index: `" <> text i <> "` in array: " <> text xs

-- Convert any Show instance to Text
text :: Show a => a -> T.Text
text = T.pack . show
