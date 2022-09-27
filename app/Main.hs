module Main where

import Parse (ParseConf (..))
import Process (processFiles)

main :: IO ()
main = do
  debtSummary <- processFiles defaultConfig "test1.csv" "test2.csv"
  print debtSummary

-- TODO: Pull this from CLI opts or something
defaultConfig :: ParseConf
defaultConfig =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 5,
      dataStartRow = 1
    }
