module Main where

import Process (processFiles)

main :: IO ()
main = do
  debtSummary <- processFiles "" ""
  putStrLn "Nothin' yet"
