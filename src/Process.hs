module Process (process, processFiles) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Expense (processExpenses)
import Parse (ParseConf (..), parse)
import Summary (MonthlyDebtSummary, summarizeDebt)

-- Process given two filepaths, one for each person.
processFiles :: ParseConf -> FilePath -> FilePath -> IO [MonthlyDebtSummary]
processFiles conf f1path f2path = do
  f1 <- TIO.readFile f1path
  f2 <- TIO.readFile f2path
  return (process conf f1 f2)

-- Tie it all together! Given two CSVs (as Text) with two people's data, return
-- the summary of who owes whom, month to month.
-- TODO: error cases, probably an Either here
process :: ParseConf -> T.Text -> T.Text -> [MonthlyDebtSummary]
process conf p1csv p2csv = summarizeDebt p1expenses p2expenses
  where
    p1expenses = processExpenses $ parse conf p1csv
    p2expenses = processExpenses $ parse conf p2csv
