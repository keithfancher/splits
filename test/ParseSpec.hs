module ParseSpec (spec) where

import Expense (Date (..), Expense (..))
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    it "returns an empty list given an empty string" $ do
      parse simpleConf "" `shouldBe` Right []

    it "parses a correctly-formed CSV" $ do
      parse simpleConf simpleCsv `shouldBe` Right simpleResult

    it "parses a correctly-formed CSV that includes a header" $ do
      parse headerConf csvWithHeader `shouldBe` Right simpleResult

  describe "parseLine" $ do
    it "parses a correctly-formed CSV line" $ do
      parseLine simpleConf simpleCsvLine `shouldBe` Right simpleLineResult

-- TODO: parse failure cases

simpleConf =
  ParseConf
    { colSep = ";",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 0 -- data starts immediately
    }

headerConf =
  ParseConf
    { colSep = ";",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 1 -- there's a header with col names before the data
    }

simpleCsv = "08/06/2022;BIG BURGERZ;-50.34\n08/31/2022;BIG BURGERZ;-93.21\n01/23/2023;STUFFZ;300"

-- Same as above, but with a header row
csvWithHeader = "DATE;DESCRIPTION;AMOUNT\n08/06/2022;BIG BURGERZ;-50.34\n08/31/2022;BIG BURGERZ;-93.21\n01/23/2023;STUFFZ;300"

simpleResult =
  [ Expense (Date 2022 08 06) (-50.34),
    Expense (Date 2022 08 31) (-93.21),
    Expense (Date 2023 01 23) 300
  ]

simpleCsvLine = "08/06/2022;BIG BURGERZ;-50.34"

simpleLineResult = Expense (Date 2022 08 06) (-50.34)
