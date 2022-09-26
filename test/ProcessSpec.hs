module ProcessSpec (spec) where

import Expense (YearAndMonth (..))
import Process
import Summary (DebtOutcome (..), MonthlyDebtSummary (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "process" $ do
    it "processes and summarizes two well-formed sets of debts correctly!" $ do
      process csv1 csv2 `shouldBe` expectedSummaries

csv1 = "08/06/2022;BIG BURGERZ;-50\n08/31/2022;BIG BURGERZ;-93\n01/23/2023;STUFFZ;300"

csv2 = "08/28/2022;BIG BURGERZ;-100\n09/12/2022;BIG BURGERZ;-666\n01/15/2023;STUFFZ;9000"

expectedSummaries =
  [ MonthlyDebtSummary (YearAndMonth 2022 08) P2OwesP1 243 21.5,
    MonthlyDebtSummary (YearAndMonth 2022 09) P1OwesP2 0 0,
    MonthlyDebtSummary (YearAndMonth 2022 10) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2022 11) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2022 12) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 01) P1OwesP2 0 0
  ]
