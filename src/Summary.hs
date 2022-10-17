module Summary
  ( DebtOutcome (..),
    MonthlyDebtSummary (..),
    normalizeTotals,
    showSummariesWithNames,
    singleMonthSummary,
    summarizeDebt,
  )
where

import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Error (Error, ErrorType (..), mkError)
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
showSummariesWithNames n1 n2 = foldl' concatSum ""
  where
    concatSum str summary = mconcat [str, showWithNames summary] -- just for folding!
    showWithNames = showSummaryWithNames n1 n2

-- Don't actually round the values when computing, but round to two decimal
-- places when showing as a String.
showRounded :: Double -> String
showRounded = printf "% 8.2f" -- pad with spaces, 8 total width, round to 2 precision

-- The three possible scenarios for a given month
data DebtOutcome = P1OwesP2 | P2OwesP1 | ExpensesEqual
  deriving (Show, Eq)

-- Given two totals, get the corresponding DebtOutcome
debtOutcome :: Ord a => a -> a -> DebtOutcome
debtOutcome total1 total2 = case compare total1 total2 of
  LT -> P1OwesP2
  GT -> P2OwesP1
  EQ -> ExpensesEqual

-- If we have names defined, can make the output a little easier to parse
showOutcomeWithNames :: String -> String -> DebtOutcome -> String
showOutcomeWithNames n1 n2 P1OwesP2 = mconcat [n1, " owes ", n2]
showOutcomeWithNames n1 n2 P2OwesP1 = mconcat [n2, " owes ", n1]
showOutcomeWithNames _ _ ExpensesEqual = "Expenses equal!"

-- The main thing. Given a list of monthly totals, one for each person,
-- summarizes the debts owed between the two people for that time span.
summarizeDebt :: [MonthlyTotal] -> [MonthlyTotal] -> Either Error [MonthlyDebtSummary]
summarizeDebt [] [] = Right [] -- both empty, nothing to do
summarizeDebt p1totals p2totals = mapM summarizeMonth zippedTotals
  where
    -- We know the *combined* list is not empty -- we've handled the "two empty
    -- lists" case above -- so `fromList` is safe here:
    (lowerBound, upperBound) = getDateBounds $ NE.fromList $ p1totals ++ p2totals
    p1normalized = normalizeTotals lowerBound upperBound p1totals
    p2normalized = normalizeTotals lowerBound upperBound p2totals
    zippedTotals = zip p1normalized p2normalized
    summarizeMonth (t1, t2) = singleMonthSummary t1 t2

-- Given a (non-empty!) list of totals, get the earliest and latest
-- YearAndMonths in that list as a tuple.
getDateBounds :: NE.NonEmpty MonthlyTotal -> (YearAndMonth, YearAndMonth)
getDateBounds totals = (lowerBound, upperBound)
  where
    lowerBound = yearAndMonth (NE.head sortedTotals)
    upperBound = yearAndMonth (NE.last sortedTotals)
    sortedTotals = sortTotalsByMonth totals
    sortTotalsByMonth = NE.sortBy compareMonth
    compareMonth t1 t2 = compare (yearAndMonth t1) (yearAndMonth t2)

-- Given two monthly totals, one for each person, calculate that month's
-- summary. Who owes whom and how much. Plus a bit of validation to make sure
-- the data is sane.
singleMonthSummary :: MonthlyTotal -> MonthlyTotal -> Either Error MonthlyDebtSummary
singleMonthSummary t1 t2
  | not (sameMonth t1 t2) = Left diffMonthErr
  | not (sameSign t1 t2) = Left diffSignErr
  | otherwise =
      Right
        ( MonthlyDebtSummary
            (yearAndMonth t1)
            (debtOutcome p1total p2total)
            combinedTotal
            amtOwed
            p1total
            p2total
        )
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
    -- If they've got different months, it's a bug. Fail rather than give possibly-suspect results:
    diffMonthErr = mkError InternalError "Totals from different months cannot be compared"
    -- I'll have to think more about this limitation. We can't currently know
    -- whether a given dataset uses positives or negatives for
    -- debits/credits. If they're different signs, it could be because one
    -- person only has credits that month OR it could be that the two
    -- datasets use different representations. Config option to fix this?
    diffSignErr = mkError InternalError "Monthly totals of different signs cannot be compared"

-- Are two monthly totals from the same month?
sameMonth :: MonthlyTotal -> MonthlyTotal -> Bool
sameMonth t1 t2 = yearAndMonth t1 == yearAndMonth t2

-- Return True if we consider the `total` field of these totals to be the same
-- sign, or equivalent. For example, both positive or both negative. Note that
-- zero is "neutral" here, is compatible with either positive or negative.
sameSign :: MonthlyTotal -> MonthlyTotal -> Bool
sameSign t1 t2
  | s1 == s2 = True -- Actually the same sign
  | s1 == 0 || s2 == 0 = True -- Either one is zero
  | otherwise = False
  where
    s1 = sign t1
    s2 = sign t2
    sign = signum . total

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
    -- Calling min/max ensures it'll work even with args reversed, removes need for error checking these:
    trueMin = min minMonth maxMonth
    trueMax = max minMonth maxMonth
    -- Months are generated in descending order, so reverse it
    months = reverse (generateMonths trueMax (NE.fromList [trueMin]))
    totals = zip months (repeat 0)

-- Convenience! Build the object from a tuple of its members.
totalFromTup :: (YearAndMonth, Double) -> MonthlyTotal
totalFromTup (ym, tot) = MonthlyTotal ym tot

-- Recursively generate a list of months (and years), given an upper bound and
-- a starting list of months. Note that this list goes in descending order,
-- mostly to make it easy to prepend the next value as we go.
generateMonths :: YearAndMonth -> NE.NonEmpty YearAndMonth -> [YearAndMonth]
generateMonths upperBound months =
  if latestMonth == upperBound -- then we're done
    then NE.toList months
    else generateMonths upperBound (nextMonth `NE.cons` months)
  where
    latestMonth = NE.head months
    nextMonth = incrementMonth latestMonth
