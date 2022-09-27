module Parse
  ( parse,
    parseLine,
    ParseConf (..),
  )
where

import qualified Data.Text as T
import Expense (Date (..), Expense (..))

data ParseConf = ParseConf
  { -- Character(s) that separate the columns in the CSV:
    colSep :: T.Text,
    -- Zero-indexed column number of the date of expense
    dateColNum :: Int,
    -- Zero-indexed column number of the amount of expense
    amountColNum :: Int,
    -- Zero-indexed row number of the beginning of the data (i.e. if there's a
    -- header row at `0`, this value might be `1`)
    dataStartRow :: Int
  }

-- Given a CSV (as Text), parse it out into a list of `Expense` objects. We'll
-- need some config data to know exactly how to parse out the data we need.
parse :: ParseConf -> T.Text -> [Expense]
parse conf expensesCsv = map parseWithConf linesWithoutHeader
  where
    parseWithConf = parseLine conf -- partial application magic!
    linesWithoutHeader = drop (dataStartRow conf) (T.lines expensesCsv)

--  One row from the CSV. Parse out a single `Expense` object.
parseLine :: ParseConf -> T.Text -> Expense
parseLine conf csvLine = Expense date amount
  where
    splitText = T.splitOn (colSep conf) csvLine
    amountText = splitText !! amountColNum conf
    date = parseDate (splitText !! dateColNum conf)
    amount = read (T.unpack amountText) :: Double -- TODO: Is there a better way?

-- TODO: This is extremely specific to my own data right now, with basically no
-- room for error. Probably use a library to do this more flexibly.
parseDate :: T.Text -> Date
parseDate dateText = Date year month day
  where
    dateSeparator = "/"
    splitText = T.splitOn dateSeparator dateText
    year = toInt 2
    month = toInt 0
    day = toInt 1
    toInt index = read $ T.unpack $ splitText !! index :: Int
