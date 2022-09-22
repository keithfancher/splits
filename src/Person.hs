module Person where

import Expense (MonthlyTotal (MonthlyTotal), YearAndMonth, incrementMonth)

data Person = Person
  { name :: String, -- TODO: text? Not important, just to differentiate
    expenseSummary :: [MonthlyTotal]
  }
  deriving (Show, Eq)

-- Summary for the shared expense for a single month
data MonthlyDebtSummary = MonthlyDebtSummary
  { yearAndMonth :: YearAndMonth,
    ower :: Person, -- The one who owes money
    owee :: Person, -- The one who is owed money
    totalPaid :: Double, -- The total paid b/w all parties
    amountOwed :: Double -- The amount the ower owes the owee :'(
  }
  deriving (Show, Eq)

summarizeDebt :: Person -> Person -> [MonthlyDebtSummary]
summarizeDebt p1 p2 = []

-- Generate a "stub" of monthly totals, all zeroed, from the beginning month to
-- ending month specified (inclusive).
generateStubTotals :: YearAndMonth -> YearAndMonth -> [MonthlyTotal]
generateStubTotals minMonth maxMonth = map totalFromTup totals
  where
    months = reverse (generateMonths maxMonth [minMonth]) -- Months are generated in descending order, so reverse it
    totals = zip months (repeat 0)
    totalFromTup (yearAndMonth, total) = MonthlyTotal yearAndMonth total

-- Recursively generate a list of months (and years), given an upper bound and
-- a starting list of months. Note that this list goes in descending order,
-- mostly to make it easy to prepend the next value as we go.
generateMonths :: YearAndMonth -> [YearAndMonth] -> [YearAndMonth]
generateMonths upperBound months =
  if latestMonth == upperBound -- then we're done
    then months
    else generateMonths upperBound (nextMonth : months)
  where
    latestMonth = head months
    nextMonth = incrementMonth latestMonth
