module PersonSpec (spec) where

import Expense (MonthlyTotal (..), YearAndMonth (..))
import Person
import Test.Hspec

spec :: Spec
spec = do
  describe "summarizeDebt" $ do
    it "returns an empty list given two people with no debt" $ do
      summarizeDebt [] [] `shouldBe` []

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

  describe "normalizeTotals" $ do
    it "returns zeroed out list given empty totals" $ do
      normalizeTotals begin end [] `shouldBe` expectedEmptyTotals

    it "returns a zero-padded list with proper endpoints given non-empty totals" $ do
      normalizeTotals begin end someTotals `shouldBe` expectedNormalizedTotals

-- TODO: test for (and handle) bad input, e.g. starting month AFTER end month

begin = YearAndMonth 2022 09

end = YearAndMonth 2023 04

someTotals =
  [ MonthlyTotal (YearAndMonth 2022 11) 42,
    MonthlyTotal (YearAndMonth 2023 02) 666,
    MonthlyTotal (YearAndMonth 2023 03) 965.21
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
