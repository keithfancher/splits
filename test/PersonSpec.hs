module PersonSpec (spec) where

import Expense (MonthlyTotal (..), YearAndMonth (..))
import Person
import Test.Hspec

spec :: Spec
spec = do
  describe "summarizeDebt" $ do
    it "returns an empty list given Persons with no debt" $ do
      summarizeDebt (Person "Alice" []) (Person "Bob" []) `shouldBe` []

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
