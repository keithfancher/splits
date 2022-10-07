module ExpenseSpec (spec) where

import Expense
import Test.Hspec

spec :: Spec
spec = do
  describe "processExpense" $ do
    it "returns an empty list given an empty list" $ do
      processExpenses [] `shouldBe` []

    it "returns an ordered list of correct monthly expenses" $ do
      processExpenses unorderedExpenses `shouldBe` expectedOrderedResults

    it "handles negative expense values" $ do
      processExpenses negativeExpenses `shouldBe` expectedNegativeResults

    it "doesn't care about weird dates, since that doesn't matter" $ do
      processExpenses weirdDateExpenses `shouldBe` expectedWeirdDateResults

-- TODO: Could also throw in some QuickCheck tests in addition to the explicit in/out pairs...
unorderedExpenses :: [Expense]
unorderedExpenses = [e4, e3, e2, e1]
  where
    e1 = Expense (Date 2000 1 1) 42.5
    e2 = Expense (Date 2000 1 15) 20.5
    e3 = Expense (Date 2000 2 5) 30.5
    e4 = Expense (Date 2020 1 1) 9

expectedOrderedResults :: [MonthlyTotal]
expectedOrderedResults =
  [ MonthlyTotal (YearAndMonth 2000 1) 63.0,
    MonthlyTotal (YearAndMonth 2000 2) 30.5,
    MonthlyTotal (YearAndMonth 2020 1) 9.0
  ]

negativeExpenses :: [Expense]
negativeExpenses = [e4, e3, e2, e1]
  where
    e1 = Expense (Date 2000 1 1) 42.5
    e2 = Expense (Date 2000 1 15) (-20.5)
    e3 = Expense (Date 2000 2 5) 30.5
    e4 = Expense (Date 2020 1 1) 9

expectedNegativeResults :: [MonthlyTotal]
expectedNegativeResults =
  [ MonthlyTotal (YearAndMonth 2000 1) 22.0,
    MonthlyTotal (YearAndMonth 2000 2) 30.5,
    MonthlyTotal (YearAndMonth 2020 1) 9.0
  ]

weirdDateExpenses :: [Expense]
weirdDateExpenses = [e4, e3, e2, e1]
  where
    e1 = Expense (Date (-40000) 56 100) 42.5 -- this is fine
    e2 = Expense (Date (-40000) 56 9000) 20.5 -- our logic doesn't care
    e3 = Expense (Date 2000 2 5) 30.5
    e4 = Expense (Date 2020 1 1) 9

expectedWeirdDateResults :: [MonthlyTotal]
expectedWeirdDateResults =
  [ MonthlyTotal (YearAndMonth (-40000) 56) 63.0,
    MonthlyTotal (YearAndMonth 2000 2) 30.5,
    MonthlyTotal (YearAndMonth 2020 1) 9.0
  ]

