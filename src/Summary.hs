module Summary
  ( DebtOutcome (..),
    MonthlyDebtSummary (..),
    normalizeTotals,
    showSummariesWithNames,
    singleMonthSummary,
    summarizeDebt,
  )
where

import qualified Data.Map as Map
import Expense (MonthlyTotal (..), YearAndMonth, incrementMonth)
import Text.Printf (printf)

-- Summary for the shared expense for a single month. Note that the amounts
-- here will (should) always be positive. e.g. `totalPaid: 300`, `amountOwed:
-- 34` means exactly what it sounds like, regardless of how debits/credits were
-- represented in the incoming data.
data MonthlyDebtSummary = MonthlyDebtSummary
  { month :: YearAndMonth,
    outcome :: DebtOutcome,
    totalPaid :: Double, -- The total paid b/w all parties
    amountOwed :: Double, -- The amount the ower owes the owee :'(
    p1Total :: Double, -- The total "person1" paid this month
    p2Total :: Double -- Ditto for "person2"
  }
  deriving (Eq, Show)

-- If we have names defined, can make the output a little easier to parse
showSummaryWithNames :: String -> String -> MonthlyDebtSummary -> String
showSummaryWithNames n1 n2 summary =
  mconcat
    [ "Month: ",
      show $ month summary,
      "\tAlice paid: ",
      showRounded $ p1Total summary,
      "\tBob paid: ",
      showRounded $ p2Total summary,
      "\tCombined expenses: ",
      showRounded $ totalPaid summary,
      "\tResult: ",
      showOutcomeWithNames n1 n2 (outcome summary),
      "\t",
      showRounded $ amountOwed summary,
      " owed\n"
    ]

-- Just stringify and concat
showSummariesWithNames :: String -> String -> [MonthlyDebtSummary] -> String
showSummariesWithNames n1 n2 = foldl concatSum ""
  where
    concatSum str summary = mconcat [str, showWithNames summary] -- just for folding!
    showWithNames = showSummaryWithNames n1 n2

-- Don't actually round the values when computing, but round to two decimal
-- places when showing as a String.
-- TODO: Or should we round the data itself? Depends how it'll be used...
showRounded :: Double -> String
showRounded = printf "% 8.2f" -- pad with spaces, 8 total width, round to 2 precision

-- The three possible scenarios for a given month
data DebtOutcome = P1OwesP2 | P2OwesP1 | ExpensesEqual
  deriving (Show, Eq)

-- If we have names defined, can make the output a little easier to parse
showOutcomeWithNames :: String -> String -> DebtOutcome -> String
showOutcomeWithNames n1 n2 P1OwesP2 = mconcat [n1, " owes ", n2]
showOutcomeWithNames n1 n2 P2OwesP1 = mconcat [n2, " owes ", n1]
showOutcomeWithNames _ _ ExpensesEqual = "Expenses equal!"

-- The main thing. Given a list of monthly totals, one for each person,
-- summarizes the debts owed between the two people for that time span.
summarizeDebt :: [MonthlyTotal] -> [MonthlyTotal] -> [MonthlyDebtSummary]
summarizeDebt [] [] = [] -- both empty, nothing to do
summarizeDebt p1totals p2totals = map summarizeMonth zippedTotals
  where
    (lowerBound, upperBound) = minAndMaxMonths p1totals p2totals
    p1normalized = normalizeTotals lowerBound upperBound p1totals
    p2normalized = normalizeTotals lowerBound upperBound p2totals
    zippedTotals = zip p1normalized p2normalized
    summarizeMonth (t1, t2) = singleMonthSummary t1 t2

-- Given two monthly totals, one for each person, calculate that month's
-- summary. Who owes whom and how much.
--
-- TODO: If two monthly totals have different signs, throw an error. There's no
-- way for us to know which number is greater, because we can't know whether
-- the dataset uses positives or negatives for debits/credits. Perhaps a config
-- option to get around this?
singleMonthSummary :: MonthlyTotal -> MonthlyTotal -> MonthlyDebtSummary
singleMonthSummary t1 t2 =
  MonthlyDebtSummary
    (yearAndMonth t1) -- TODO: verify both same month, or error?
    (debtOutcome p1total p2total)
    combinedTotal
    amtOwed
    p1total
    p2total
  where
    -- Note the call to `abs`. Need to normalize here so we can consistently
    -- decide who owes whom, regardless of whether credits or debits are
    -- represented as positive or negative numbers. (For example, if the two
    -- totals are -100 and -200, the latter paid "more" that month even though
    -- it's the smaller number.)
    p1total = abs $ total t1
    p2total = abs $ total t2
    combinedTotal = p1total + p2total
    -- For whoever paid less, the diff between half and the amount they paid:
    amtOwed = (combinedTotal / 2) - min p1total p2total
    debtOutcome p1t p2t
      | p1t < p2t = P1OwesP2
      | p1t > p2t = P2OwesP1
      | otherwise = ExpensesEqual

-- Given two list of expenses, calculate the lower and upper bound of the
-- months. Used to normalize the expense lists. Note that it doesn't make sense
-- to call this with two empty lists -- result is undefined.
-- TODO: handle case of two empty lists... use an Either? explicitly return an error?
-- TODO: verify sorted, or sort? Probably best to ensure sorted initially
minAndMaxMonths :: [MonthlyTotal] -> [MonthlyTotal] -> (YearAndMonth, YearAndMonth)
minAndMaxMonths p1totals [] = (yearAndMonth (head p1totals), yearAndMonth (last p1totals))
minAndMaxMonths [] p2totals = (yearAndMonth (head p2totals), yearAndMonth (last p2totals))
minAndMaxMonths p1totals p2totals = (lowYearAndMonth, highYearAndMonth)
  where
    lowYearAndMonth = min (head p1dates) (head p2dates)
    highYearAndMonth = max (last p1dates) (last p2dates)
    p1dates = map yearAndMonth p1totals
    p2dates = map yearAndMonth p2totals

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
-- TODO: just make this work even if args are reversed, avoid the need for error checking altogether
generateStubTotals :: YearAndMonth -> YearAndMonth -> [MonthlyTotal]
generateStubTotals minMonth maxMonth = map totalFromTup totals
  where
    months = reverse (generateMonths maxMonth [minMonth]) -- Months are generated in descending order, so reverse it
    totals = zip months (repeat 0)

-- Convenience! Build the object from a tuple of its members.
totalFromTup :: (YearAndMonth, Double) -> MonthlyTotal
totalFromTup (ym, tot) = MonthlyTotal ym tot

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
