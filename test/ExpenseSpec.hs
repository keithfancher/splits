module ExpenseSpec where

import Expense
import Test.Hspec

spec :: Spec
spec = do
  describe "That thing" $ do
    it "adds 2 + 2 and gets 4" $ do
      (2 + 2) `shouldBe` 4
