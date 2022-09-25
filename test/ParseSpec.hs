module ParseSpec (spec) where

import Expense (Date (..), Expense (..))
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    it "returns an empty list given an empty string" $ do
      parse "" sep 0 0 `shouldBe` []

    it "parses a correctly-formed CSV" $ do
      parse simpleCsv sep 0 2 `shouldBe` simpleResult

  describe "parseLine" $ do
    it "parses a correctly-formed CSV line" $ do
      parseLine sep 0 2 simpleCsvLine `shouldBe` simpleLineResult

sep = ";"

simpleCsv = "08/06/2022;BIG BURGERZ;-50.34\n08/31/2022;BIG BURGERZ;-93.21\n01/23/2023;STUFFZ;300"

simpleResult =
  [ Expense (Date 2022 08 06) (-50.34),
    Expense (Date 2022 08 31) (-93.21),
    Expense (Date 2023 01 23) 300
  ]

simpleCsvLine = "08/06/2022;BIG BURGERZ;-50.34"

simpleLineResult = Expense (Date 2022 08 06) (-50.34)
