module Error
  ( Error (..),
    ErrorType (..),
    is,
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

data ErrorType = ParseError | InvalidInput | FileNotFound | InternalError
  deriving (Show, Eq)

-- Slight convenience!
mkError :: ErrorType -> T.Text -> Error
mkError e m = Error e (Just m)

printError :: Error -> IO ()
printError = TIO.putStrLn . showError

showError :: Error -> T.Text
showError (Error e m) =
  "Error! " <> eText <> case m of
    Just s -> ": \n  " <> s
    Nothing -> ""
  where
    eText = T.pack (show e)

-- Check whether a return value contains an error of the expected type. Useful
-- because we don't always want to verify the error *message*, just the type.
-- This is particularly handy when combined with hspec's `shouldSatisfy`,
-- thanks to the magic of partial application.
is :: ErrorType -> Either Error a -> Bool
is expectedType retVal = case retVal of
  Left (Error t _) | t == expectedType -> True
  _ -> False
