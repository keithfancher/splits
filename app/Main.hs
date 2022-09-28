module Main where

import Parse (ParseConf (..))
import Process (processFiles)
import Summary (showSummariesWithNames)

main :: IO ()
main = do
  debtSummary <- processFiles defaultConfig "test/data/user1-test.csv" "test/data/user2-test.csv"
  putStrLn (showSummariesWithNames "Alice" "Bob" debtSummary)

-- TODO: Pull this from CLI opts or something
defaultConfig :: ParseConf
defaultConfig =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 5,
      dataStartRow = 1
    }
