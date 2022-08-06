module PersonSpec (spec) where

import Person
import Test.Hspec

spec :: Spec
spec = do
  describe "summarizeDebt" $ do
    it "returns an empty list given Persons with no debt" $ do
      summarizeDebt (Person "Alice" []) (Person "Bob" []) `shouldBe` []
