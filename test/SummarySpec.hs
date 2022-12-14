module SummarySpec (spec) where

import Error (ErrorType (..), is)
import Expense (MonthlyTotal (..), YearAndMonth (..))
import Summary
import Test.Hspec

spec :: Spec
spec = do
  describe "summarizeDebt" $ do
    it "returns an empty list given two people with no debt" $ do
      summarizeDebt [] [] `shouldBe` Right []

    it "summarizes a mixture of debts for two people" $ do
      summarizeDebt someTotals otherTotals `shouldBe` Right expectedSummaries

    it "summarizes a one-sided debt for two people, only one person paid" $ do
      summarizeDebt someTotals [] `shouldBe` Right oneSidedSummaries

  describe "singleMonthSummary" $ do
    it "figures out how much person1 owes person2" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) 500)
        (MonthlyTotal (YearAndMonth 2022 04) 600)
        `shouldBe` Right (MonthlyDebtSummary (YearAndMonth 2022 04) P1OwesP2 1100 50 500 600)

    it "figures out how much person2 owes person1" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) 1100)
        (MonthlyTotal (YearAndMonth 2022 04) 45)
        `shouldBe` Right (MonthlyDebtSummary (YearAndMonth 2022 04) P2OwesP1 1145 527.5 1100 45)

    it "knows when person1 and person2 are even-steven" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) 1100)
        (MonthlyTotal (YearAndMonth 2022 04) 1100)
        `shouldBe` Right (MonthlyDebtSummary (YearAndMonth 2022 04) ExpensesEqual 2200 0 1100 1100)

    -- This is a tricky one. Some datasets use negatives to represent expenses,
    -- some use positives. We need to make sure the "who owes whom" logic works
    -- in either case. We use positive numbers for our totals internally.
    it "compares correctly when expenses are represented as negative numbers" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) (-1300))
        (MonthlyTotal (YearAndMonth 2022 04) (-1100))
        `shouldBe` Right (MonthlyDebtSummary (YearAndMonth 2022 04) P2OwesP1 2400 100 1300 1100)

    it "compares correctly when one person has zero expenses that month" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) (-1300))
        (MonthlyTotal (YearAndMonth 2022 04) 0)
        `shouldBe` Right (MonthlyDebtSummary (YearAndMonth 2022 04) P2OwesP1 1300 650 1300 0)

    it "fails with `InternalError` when two summaries are from different months" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) (-1300))
        (MonthlyTotal (YearAndMonth 2022 07) (-1100))
        `shouldSatisfy` is InternalError

    it "fails with `InternalError` when two summaries have different signs" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) (-1300))
        (MonthlyTotal (YearAndMonth 2022 04) 1100)
        `shouldSatisfy` is InternalError

  describe "normalizeTotals" $ do
    it "returns zeroed out list given empty totals" $ do
      normalizeTotals (YearAndMonth 2022 09) (YearAndMonth 2023 04) []
        `shouldBe` expectedEmptyTotals

    it "returns a zero-padded list with proper endpoints given non-empty totals" $ do
      normalizeTotals (YearAndMonth 2022 09) (YearAndMonth 2023 04) someTotals
        `shouldBe` expectedNormalizedTotals

    it "works even if min and max args are reversed" $ do
      normalizeTotals (YearAndMonth 2023 04) (YearAndMonth 2022 09) someTotals
        `shouldBe` expectedNormalizedTotals

someTotals :: [MonthlyTotal]
someTotals =
  [ MonthlyTotal (YearAndMonth 2022 11) 42,
    MonthlyTotal (YearAndMonth 2023 02) 666,
    MonthlyTotal (YearAndMonth 2023 03) 965.21
  ]

otherTotals :: [MonthlyTotal]
otherTotals =
  [ MonthlyTotal (YearAndMonth 2022 11) 50,
    MonthlyTotal (YearAndMonth 2023 02) 700,
    MonthlyTotal (YearAndMonth 2023 03) 400,
    MonthlyTotal (YearAndMonth 2023 05) 250
  ]

-- someTotals + otherTotals:
expectedSummaries :: [MonthlyDebtSummary]
expectedSummaries =
  [ MonthlyDebtSummary (YearAndMonth 2022 11) P1OwesP2 92 4 42 50,
    MonthlyDebtSummary (YearAndMonth 2022 12) ExpensesEqual 0 0 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 1) ExpensesEqual 0 0 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 2) P1OwesP2 1366 17 666 700,
    MonthlyDebtSummary (YearAndMonth 2023 3) P2OwesP1 1365.21 282.605 965.21 400,
    MonthlyDebtSummary (YearAndMonth 2023 4) ExpensesEqual 0 0 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 5) P1OwesP2 250 125 0 250
  ]

-- someTotals + []
oneSidedSummaries :: [MonthlyDebtSummary]
oneSidedSummaries =
  [ MonthlyDebtSummary (YearAndMonth 2022 11) P2OwesP1 42 21 42 0,
    MonthlyDebtSummary (YearAndMonth 2022 12) ExpensesEqual 0 0 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 1) ExpensesEqual 0 0 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 02) P2OwesP1 666 333 666 0,
    MonthlyDebtSummary (YearAndMonth 2023 03) P2OwesP1 965.21 482.605 965.21 0
  ]

expectedNormalizedTotals :: [MonthlyTotal]
expectedNormalizedTotals =
  [ MonthlyTotal (YearAndMonth 2022 09) 0,
    MonthlyTotal (YearAndMonth 2022 10) 0,
    MonthlyTotal (YearAndMonth 2022 11) 42,
    MonthlyTotal (YearAndMonth 2022 12) 0,
    MonthlyTotal (YearAndMonth 2023 01) 0,
    MonthlyTotal (YearAndMonth 2023 02) 666,
    MonthlyTotal (YearAndMonth 2023 03) 965.21,
    MonthlyTotal (YearAndMonth 2023 04) 0
  ]

expectedEmptyTotals :: [MonthlyTotal]
expectedEmptyTotals =
  [ MonthlyTotal (YearAndMonth 2022 09) 0,
    MonthlyTotal (YearAndMonth 2022 10) 0,
    MonthlyTotal (YearAndMonth 2022 11) 0,
    MonthlyTotal (YearAndMonth 2022 12) 0,
    MonthlyTotal (YearAndMonth 2023 01) 0,
    MonthlyTotal (YearAndMonth 2023 02) 0,
    MonthlyTotal (YearAndMonth 2023 03) 0,
    MonthlyTotal (YearAndMonth 2023 04) 0
  ]
