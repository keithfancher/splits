module ParseSpec (spec) where

import Error (ErrorType (..), is)
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

    it "parses a correctly-formed CSV that includes quoted fields" $ do
      parse quotedConf csvWithQuotedFields `shouldBe` Right simpleResult

  describe "parseLine" $ do
    it "parses a correctly-formed CSV line" $ do
      parseLine simpleConf simpleCsvLine `shouldBe` Right simpleLineResult

    it "parses a correctly-formed CSV line with date in YMD format" $ do
      parseLine ymdConf simpleCsvLineYmdDate `shouldBe` Right simpleLineResult

    it "parses a correctly-formed CSV line with date in DMY format" $ do
      parseLine dmyConf simpleCsvLineDmyDate `shouldBe` Right simpleLineResult

    it "returns a ParseError when parsing an invalid `amount`" $ do
      parseLine badAmountConf simpleCsvLine `shouldSatisfy` is ParseError

    it "returns an InvalidInput error when given an out-of-bounds index" $ do
      parseLine outOfBoundsConf simpleCsvLine `shouldSatisfy` is InvalidInput

    it "returns an InvalidInput error when parsing an invalid date" $ do
      parseLine badDateConf simpleCsvLine `shouldSatisfy` is InvalidInput

-- Our default date config:
mdy :: DateParseConf
mdy = DateParseConf MDY "/"

simpleConf :: ParseConf
simpleConf =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 0, -- data starts immediately
      dateConf = mdy
    }

headerConf :: ParseConf
headerConf =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 1, -- there's a header with col names before the data
      dateConf = mdy
    }

quotedConf :: ParseConf
quotedConf =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 0, -- data starts immediately
      dateConf = DateParseConf MDY "," -- commas to separate date fields? weird!
    }

ymdConf :: ParseConf
ymdConf =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 0,
      dateConf = DateParseConf YMD "-" -- different date format, "Y-M-D"
    }

dmyConf :: ParseConf
dmyConf =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 0,
      dateConf = DateParseConf DMY "." -- different date format, "D.M.Y"
    }

badAmountConf :: ParseConf
badAmountConf =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 1, -- this ain't right! 1 is the description
      dataStartRow = 0,
      dateConf = mdy
    }

outOfBoundsConf :: ParseConf
outOfBoundsConf =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 20, -- out of bounds!
      dataStartRow = 0,
      dateConf = mdy
    }

badDateConf :: ParseConf
badDateConf =
  ParseConf
    { colSep = ",",
      dateColNum = 1, -- this is the index for description, not date!
      amountColNum = 2,
      dataStartRow = 0,
      dateConf = mdy
    }

simpleCsv :: CSVRaw
simpleCsv = "08/06/2022,BIG BURGERZ,-50.34\n08/31/2022,BIG BURGERZ,-93.21\n01/23/2023,STUFFZ,300"

-- Same as above, but with a header row
csvWithHeader :: CSVRaw
csvWithHeader = "DATE,DESCRIPTION,AMOUNT\n08/06/2022,BIG BURGERZ,-50.34\n08/31/2022,BIG BURGERZ,-93.21\n01/23/2023,STUFFZ,300"

-- A CSV whose fields contain commas, so must be quoted
csvWithQuotedFields :: CSVRaw
csvWithQuotedFields = "\"08,06,2022\",BIG BURGERZ,-50.34\n\"08,31,2022\",BIG BURGERZ,-93.21\n\"01,23,2023\",STUFFZ,300"

simpleResult :: [Expense]
simpleResult =
  [ Expense (Date 2022 08 06) (-50.34),
    Expense (Date 2022 08 31) (-93.21),
    Expense (Date 2023 01 23) 300
  ]

simpleCsvLine :: CSVLine
simpleCsvLine = ["08/06/2022", "BIG BURGERZ", "-50.34"]

-- Same data as above, but date in a diff format:
simpleCsvLineYmdDate :: CSVLine
simpleCsvLineYmdDate = ["2022-08-06", "BIG BURGERZ", "-50.34"]

-- ...and one more time, a different date format:
simpleCsvLineDmyDate :: CSVLine
simpleCsvLineDmyDate = ["6.8.2022", "BIG BURGERZ", "-50.34"]

simpleLineResult :: Expense
simpleLineResult = Expense (Date 2022 08 06) (-50.34)
