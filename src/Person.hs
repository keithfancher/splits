module Person where

import qualified Data.Map as Map
import Expense (MonthlyTotal (..), YearAndMonth, incrementMonth)

data Person = Person
  { name :: String, -- TODO: text? Not important, just to differentiate
    expenseSummary :: [MonthlyTotal]
  }
  deriving (Show, Eq)

-- Summary for the shared expense for a single month
data MonthlyDebtSummary = MonthlyDebtSummary
  { --yearAndMonth :: YearAndMonth, -- TODO: rename, this clashes (or look into lang ext?)
    ower :: Person, -- The one who owes money
    owee :: Person, -- The one who is owed money
    totalPaid :: Double, -- The total paid b/w all parties
    amountOwed :: Double -- The amount the ower owes the owee :'(
  }
  deriving (Show, Eq)

summarizeDebt :: Person -> Person -> [MonthlyDebtSummary]
summarizeDebt p1 p2 = []

-- Given two Persons and their list of expenses, calculate the lower and upper
-- bound of the months. Used to normalize the expense lists.
-- TODO: verify sorted, or sort? Probably best to ensure sorted initially
minAndMaxMonths :: Person -> Person -> (YearAndMonth, YearAndMonth)
minAndMaxMonths p1 p2 = (lowYearAndMonth, highYearAndMonth)
  where
    lowYearAndMonth = min (head p1dates) (head p2dates)
    highYearAndMonth = max (last p1dates) (last p2dates)
    p1dates = map yearAndMonth (expenseSummary p1)
    p2dates = map yearAndMonth (expenseSummary p2)

-- Given a low month and a high month, fill in all the "blanks" of a list of
-- monthly expenses, so the list is contiguous from beginning to end. Any
-- missing months will be zeros.
normalizeTotals :: YearAndMonth -> YearAndMonth -> [MonthlyTotal] -> [MonthlyTotal]
normalizeTotals minMonth maxMonth totals = map totalFromTup (Map.toList totalsMap)
  where
    totalsMap = insertTotals stubTotalMap totals
    stubTotalMap = totalsToMap (generateStubTotals minMonth maxMonth)
    insertTotals tmap [] = tmap -- empty list? we're done
    insertTotals tmap ((MonthlyTotal ym t) : xs) = insertTotals (Map.insert ym t tmap) xs

-- Builds a Map from YearAndMonth -> Total. Useful middle step for
-- building/comparing a list of totals.
totalsToMap :: [MonthlyTotal] -> Map.Map YearAndMonth Double
totalsToMap totals = Map.fromList (map toTuple totals)
  where
    toTuple (MonthlyTotal m t) = (m, t)

-- Generate a "stub" of monthly totals, all zeroed, from the beginning month to
-- ending month specified (inclusive).
generateStubTotals :: YearAndMonth -> YearAndMonth -> [MonthlyTotal]
generateStubTotals minMonth maxMonth = map totalFromTup totals
  where
    months = reverse (generateMonths maxMonth [minMonth]) -- Months are generated in descending order, so reverse it
    totals = zip months (repeat 0)

-- Convenience! Build the object from a tuple of its members.
totalFromTup :: (YearAndMonth, Double) -> MonthlyTotal
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
