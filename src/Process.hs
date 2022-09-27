module Process (process) where

import qualified Data.Text as T
import Expense (processExpenses)
import Parse (ParseConf (..), parse)
import Summary (MonthlyDebtSummary, summarizeDebt)

-- Tie it all together! Given two CSVs (as Text) with two people's data, return
-- the summary of who owes whom, month to month.
-- TODO: error cases, probably an Either here
process :: T.Text -> T.Text -> [MonthlyDebtSummary]
process p1csv p2csv = summarizeDebt p1expenses p2expenses
  where
    p1expenses = processExpenses $ parse defaultConfig p1csv
    p2expenses = processExpenses $ parse defaultConfig p2csv

-- TODO: param for this, and pull it from CLI opts or something
defaultConfig :: ParseConf
defaultConfig =
  ParseConf
    { colSep = ";",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 1
    }
