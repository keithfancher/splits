module Error
  ( Error (..),
    ErrorType (..),
    mkError,
  )
where

import qualified Data.Text as T

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
