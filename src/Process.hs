module Process (process) where

import qualified Data.Text as T
import Expense (processExpenses)
import Parse (parse)
import Summary (MonthlyDebtSummary, summarizeDebt)

-- Tie it all together! Given two CSVs (as Text) with two people's data, return
-- the summary of who owes whom, month to month.
-- TODO: CSV config object? separators, column nums etc as a data type instead
-- of annoying individual params.
-- TODO: error cases, probably an Either here
process :: T.Text -> T.Text -> [MonthlyDebtSummary]
process p1csv p2csv = summarizeDebt p1expenses p2expenses
  where
    sep = ";" -- TODO: param
    dateCol = 0 -- TODO: param
    amtCol = 2 -- TODO: param
    p1expenses = processExpenses $ parse p1csv sep dateCol amtCol
    p2expenses = processExpenses $ parse p2csv sep dateCol amtCol
