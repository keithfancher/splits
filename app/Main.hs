module Main where

import Options (CliOptions (..), cliOptParser)
import Options.Applicative (execParser)
import Parse (ParseConf (..))
import Process (processFiles)

main :: IO ()
main = do
  (CliOptions conf csvfile1 csvfile2) <- execParser cliOptParser
  putStrLn ("Reading Alice's expenses from: " ++ csvfile1)
  putStrLn ("Reading Bob's expenses from: " ++ csvfile1)
  debtSummary <- processFiles conf csvfile1 csvfile2
  case debtSummary of
    Left err -> print err
    Right summ -> print summ
