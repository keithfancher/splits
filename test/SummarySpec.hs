module SummarySpec (spec) where

import Expense (MonthlyTotal (..), YearAndMonth (..))
import Summary
import Test.Hspec

spec :: Spec
spec = do
  describe "summarizeDebt" $ do
    it "returns an empty list given two people with no debt" $ do
      summarizeDebt [] [] `shouldBe` []

    it "summarizes a mixture of debts for two people" $ do
      summarizeDebt someTotals otherTotals `shouldBe` expectedSummaries

    it "summarizes a one-sided debt for two people, only one person paid" $ do
      summarizeDebt someTotals [] `shouldBe` oneSidedSummaries

  describe "singleMonthSummary" $ do
    it "figures out how much person1 owes person2" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) 500)
        (MonthlyTotal (YearAndMonth 2022 04) 600)
        `shouldBe` MonthlyDebtSummary (YearAndMonth 2022 04) P1OwesP2 1100 50

    it "figures out how much person2 owes person1" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) 1100)
        (MonthlyTotal (YearAndMonth 2022 04) 45)
        `shouldBe` MonthlyDebtSummary (YearAndMonth 2022 04) P2OwesP1 1145 527.5

    it "knows when person1 and person2 are even-steven" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) 1100)
        (MonthlyTotal (YearAndMonth 2022 04) 1100)
        `shouldBe` MonthlyDebtSummary (YearAndMonth 2022 04) ExpensesEqual 2200 0

    -- This is a tricky one. Some datasets use negatives to represent expenses,
    -- some use positives. We need to make sure the "who owes whom" logic works
    -- in either case. We use positive numbers for our totals internally.
    it "compares correctly when expenses are represented as negative numbers" $ do
      singleMonthSummary
        (MonthlyTotal (YearAndMonth 2022 04) (-1300))
        (MonthlyTotal (YearAndMonth 2022 04) (-1100))
        `shouldBe` MonthlyDebtSummary (YearAndMonth 2022 04) P2OwesP1 2400 100

  describe "normalizeTotals" $ do
    it "returns zeroed out list given empty totals" $ do
      normalizeTotals (YearAndMonth 2022 09) (YearAndMonth 2023 04) []
        `shouldBe` expectedEmptyTotals

    it "returns a zero-padded list with proper endpoints given non-empty totals" $ do
      normalizeTotals (YearAndMonth 2022 09) (YearAndMonth 2023 04) someTotals
        `shouldBe` expectedNormalizedTotals

someTotals =
  [ MonthlyTotal (YearAndMonth 2022 11) 42,
    MonthlyTotal (YearAndMonth 2023 02) 666,
    MonthlyTotal (YearAndMonth 2023 03) 965.21
  ]

otherTotals =
  [ MonthlyTotal (YearAndMonth 2022 11) 50,
    MonthlyTotal (YearAndMonth 2023 02) 700,
    MonthlyTotal (YearAndMonth 2023 03) 400,
    MonthlyTotal (YearAndMonth 2023 05) 250
  ]

-- someTotals + otherTotals:
expectedSummaries =
  [ MonthlyDebtSummary (YearAndMonth 2022 11) P1OwesP2 92 4,
    MonthlyDebtSummary (YearAndMonth 2022 12) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 1) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 2) P1OwesP2 1366 17,
    MonthlyDebtSummary (YearAndMonth 2023 3) P2OwesP1 1365.21 282.605,
    MonthlyDebtSummary (YearAndMonth 2023 4) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 5) P1OwesP2 250 125
  ]

-- someTotals + []
oneSidedSummaries =
  [ MonthlyDebtSummary (YearAndMonth 2022 11) P2OwesP1 42 21,
    MonthlyDebtSummary (YearAndMonth 2022 12) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 1) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 02) P2OwesP1 666 333,
    MonthlyDebtSummary (YearAndMonth 2023 03) P2OwesP1 965.21 482.605
  ]

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
