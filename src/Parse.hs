module Parse
  ( parse,
    parseLine,
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
    dataStartRow :: Int
  }

-- Given a CSV (as Text), parse it out into a list of `Expense` objects. We'll
-- need some config data to know exactly how to parse out the data we need.
parse :: ParseConf -> T.Text -> Either Error [Expense]
parse conf expensesCsv = mapM parseWithConf linesWithoutHeader
  where
    parseWithConf = parseLine conf -- partial application magic!
    linesWithoutHeader = drop (dataStartRow conf) (T.lines expensesCsv)

--  One row from the CSV. Parse out a single `Expense` object.
parseLine :: ParseConf -> T.Text -> Either Error Expense
parseLine conf csvLine = Expense date <$> amount
  where
    splitText = T.splitOn (colSep conf) csvLine
    date = parseDate (splitText !! dateColNum conf)
    amountText = splitText `nth` amountColNum conf
    amount = amountText >>= parseAmount

parseAmount :: T.Text -> Either Error Double
parseAmount a = case readMaybe (T.unpack a) of
  Just amt -> Right amt
  Nothing -> Left $ mkError ParseError ("Error parsing amount: " <> a)

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

-- A (hopefully?!) safe version of `!!`. Won't die with invalid index, has a
-- maybe-useful error message.
nth :: [a] -> Int -> Either Error a
nth xs i = case drop i xs of
  x : _ -> Right x
  [] -> Left $ mkError InvalidInput msg
    where
      msg = "Index out of bounds: " <> text i <> "... check your config?"

-- Convert any Show instance to Text
text :: Show a => a -> T.Text
text = T.pack . show
