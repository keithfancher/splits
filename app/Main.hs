module Main where

import Parse (ParseConf (..))
import Process (processFiles)

main :: IO ()
main = do
  debtSummary <- processFiles defaultConfig "test/data/user1-test.csv" "test/data/user2-test.csv"
  case debtSummary of
    Left err -> print err
    Right summ -> print summ

-- TODO: Pull this from CLI opts or something
defaultConfig :: ParseConf
defaultConfig =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 5,
      dataStartRow = 1
    }
