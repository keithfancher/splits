module Parse where

import qualified Data.Text as T
import Expense (Date (..), Expense (Expense))

-- Given a CSV (as Text), parse it out into a list of `Expense` objects. We'll
-- also need to know the separator character(s) as well as the (zero-indexed)
-- column numbers for the data we want (date and amount). We can ignore the
-- other columns.
parse :: T.Text -> T.Text -> Int -> Int -> [Expense]
parse expensesCsv sep dateColNum amountColNum = map parseWithArgs (T.lines expensesCsv)
  where
    parseWithArgs = parseLine sep dateColNum amountColNum -- partial application magic!

--  One row from the CSV. Parse out a single `Expense` object.
parseLine :: T.Text -> Int -> Int -> T.Text -> Expense
parseLine sep dateColNum amountColNum csvLine = Expense date amount
  where
    splitText = T.splitOn sep csvLine
    amountText = splitText !! amountColNum
    date = parseDate (splitText !! dateColNum)
    amount = read (T.unpack amountText) :: Double -- TODO: Something like this? Is there a better way?

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
