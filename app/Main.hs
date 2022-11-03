module Main (main) where

import Error (printError)
import Options (CliOptions (..), cliOptParser)
import Options.Applicative (execParser)
import Process (processFiles)
import Summary (showSummariesWithNames)

main :: IO ()
main = do
  (CliOptions conf csvfile1 csvfile2) <- execParser cliOptParser
  putStrLn ("Reading Alice's expenses from: " ++ csvfile1)
  putStrLn ("Reading Bob's expenses from: " ++ csvfile1)
  debtSummary <- processFiles conf csvfile1 csvfile2
  case debtSummary of
    Left err -> printError err
    Right summ -> putStr (showSummariesWithNames "Alice" "Bob" summ)
