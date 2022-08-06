module Person where

import Expense (MonthlyTotal, YearAndMonth)

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
