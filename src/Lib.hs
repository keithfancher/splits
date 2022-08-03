module Lib where

-- ( someFunc,
--   Date,
--   Expense,
--   MonthlyTotal,
--   totalExpenses,
-- )

import Data.List (groupBy, sortBy)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Date = Date
  { year :: Int,
    month :: Int,
    day :: Int
  }
  deriving (Show, Eq)

-- A date with only year and month ... combine with above type? Replace?
data YearAndMonth = YearAndMonth Int Int
  deriving (Show, Eq, Ord)

data Expense = Expense
  { date :: Date,
    amount :: Double
  }
  deriving (Show, Eq)

data MonthlyTotal = MonthlyTotal
  { yearAndMonth :: YearAndMonth,
    total :: Double
  }

-- Convenience!
getMonth :: Expense -> Int
getMonth = month . date

getYear :: Expense -> Int
getYear = year . date

getYearAndMonth :: Expense -> YearAndMonth
getYearAndMonth e = YearAndMonth (getYear e) (getMonth e)

-- Do two expenses share a month? (And year, of course)
sameMonth :: Expense -> Expense -> Bool
sameMonth e1 e2 = getYearAndMonth e1 == getYearAndMonth e2

-- Sort a list of expenses by (year and) month
sortByMonth :: [Expense] -> [Expense]
sortByMonth = sortBy compareMonth
  where
    compareMonth e1 e2 = compare (getYearAndMonth e1) (getYearAndMonth e2)

-- Group all expenses from the same month into sublists
groupExpenses :: [Expense] -> [[Expense]]
groupExpenses expenses = groupBy sameMonth sortedExpenses
  where
    sortedExpenses = sortByMonth expenses

-- If two Expenses share a month, return Just (Month, Total)
-- otherwise, return Nothing
combineExpenses :: Expense -> Expense -> Maybe (Int, Double)
combineExpenses e1 e2
  | sameMonth e1 e2 = Just (getMonth e1, amount e1 + amount e2)
  | otherwise = Nothing

reduceExpenses :: [Expense] -> [Maybe (Int, Double)]
reduceExpenses [] = []
reduceExpenses (x : xs) = [Nothing] -- fold ain't gonna work, types!
-- reduceExpenses (x:xs) = foldl combineExpenses x xs

-- Add up a list of Expenses, ignoring the dates
totalExpenses :: [Expense] -> Double
totalExpenses expenses = foldl (+) 0.0 amounts
  where
    amounts = map amount expenses
