module Expense
  ( Date (..),
    Expense (..),
    incrementMonth,
    MonthlyTotal (..),
    processExpenses,
    YearAndMonth (..),
  )
where

import Control.Monad (foldM)
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe)

data Date = Date
  { year :: Int,
    month :: Int,
    day :: Int
  }
  deriving (Show, Eq)

-- A date with only year and month ... combine with above type? Replace?
data YearAndMonth = YearAndMonth Int Int
  deriving (Eq, Ord)

instance Show YearAndMonth where
  show (YearAndMonth y m) = mconcat [show y, "/", show m]

data Expense = Expense
  { date :: Date,
    -- Note that `amount` can be positive or negative. There's no way for us to
    -- know whether a dataset uses negatives to represent credits or debits. It
    -- shouldn't matter! As long as they're internally consistent, it'll even
    -- out as we add them up.
    amount :: Double
  }
  deriving (Show, Eq)

data MonthlyTotal = MonthlyTotal
  { yearAndMonth :: YearAndMonth,
    total :: Double
  }
  deriving (Show, Eq)

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

-- Given a year and month, return the following month. Accounts for the year
-- ending, etc. Note that we still aren't checking for wacky values here.
incrementMonth :: YearAndMonth -> YearAndMonth
incrementMonth (YearAndMonth y m) =
  if m == 12
    then YearAndMonth (y + 1) 1
    else YearAndMonth y (m + 1)

-- Sort a list of expenses by (year and) month
sortByMonth :: [Expense] -> [Expense]
sortByMonth = sortBy compareMonth
  where
    compareMonth e1 e2 = compare (getYearAndMonth e1) (getYearAndMonth e2)

-- Given a list of arbitrary expenses, group them by month and return a list of
-- MonthlyTotals, in ascending order.
--
-- NOTE: Not 100% sure how I want to handle the Nothings... filtering out with
-- mapMaybe for now. In theory, it can't fail based on input, only if my own
-- logic has failed.
processExpenses :: [Expense] -> [MonthlyTotal]
processExpenses exps = mapMaybe accumulateExpenses grouped
  where
    grouped = groupExpenses exps

-- Group all expenses from the same month into sublists. Note that this grouping
-- requires them to be sorted, which is a handy for the output list as well.
groupExpenses :: [Expense] -> [[Expense]]
groupExpenses expenses = groupBy sameMonth sortedExpenses
  where
    sortedExpenses = sortByMonth expenses

-- Given an expense and a monthly total, add that expense in and return the new
-- total. If they're not from the same month... Nothing.
addExpense :: MonthlyTotal -> Expense -> Maybe MonthlyTotal
addExpense t e =
  if monthsMatch
    then Just (MonthlyTotal (yearAndMonth t) newTotal)
    else Nothing
  where
    monthsMatch = getYearAndMonth e == yearAndMonth t
    newTotal = total t + amount e

-- Add up a list of Expenses into a monthly total. If the expenses aren't all
-- from the same month, return Nothing.
accumulateExpenses :: [Expense] -> Maybe MonthlyTotal
accumulateExpenses [] = Nothing
accumulateExpenses (x : xs) = foldM addExpense (expToTotal x) xs
  where
    -- to get our starting fold value, just turn the expense into a total object
    expToTotal e = MonthlyTotal (getYearAndMonth e) (amount e)
