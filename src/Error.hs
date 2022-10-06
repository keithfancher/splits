module Error
  ( Error (..),
    ErrorType (..),
    mkError,
    printError,
    showError,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Error = Error
  { err :: ErrorType,
    msg :: Maybe T.Text
  }
  deriving (Show, Eq)

data ErrorType = ParseError | InvalidInput | FileNotFound
  deriving (Show, Eq)

-- Slight convenience!
mkError :: ErrorType -> T.Text -> Error
mkError e msg = Error e (Just msg)

printError :: Error -> IO ()
printError = TIO.putStrLn . showError

showError :: Error -> T.Text
showError (Error e m) =
  "Error! " <> eText <> case m of
    Just s -> ": \n  " <> s
    Nothing -> ""
  where
    eText = T.pack (show e)
