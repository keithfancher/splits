module ProcessSpec (spec) where

import Expense (YearAndMonth (..))
import Parse (ParseConf (..))
import Process
import Summary (DebtOutcome (..), MonthlyDebtSummary (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "process" $ do
    it "processes and summarizes two (minimal) well-formed sets of debts correctly!" $ do
      process testConf simpleCsv1 simpleCsv2 `shouldBe` expectedSummaries

    it "processes and summarizes two (full-fledged) well-formed sets of debts correctly!" $ do
      process bigConf bigCsvData1 bigCsvData2 `shouldBe` bigSummaries

    it "process empty text input successfully, returning empty list" $ do
      process testConf "" "" `shouldBe` []

testConf =
  ParseConf
    { colSep = ";",
      dateColNum = 0,
      amountColNum = 2,
      dataStartRow = 0
    }

simpleCsv1 = "08/06/2022;BIG BURGERZ;-50\n08/31/2022;BIG BURGERZ;-93\n01/23/2023;STUFFZ;300"

simpleCsv2 = "08/28/2022;BIG BURGERZ;-100\n09/12/2022;BIG BURGERZ;-666\n01/15/2023;STUFFZ;9000"

expectedSummaries =
  [ MonthlyDebtSummary (YearAndMonth 2022 08) P2OwesP1 243 21.5,
    MonthlyDebtSummary (YearAndMonth 2022 09) P1OwesP2 666 333,
    MonthlyDebtSummary (YearAndMonth 2022 10) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2022 11) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2022 12) ExpensesEqual 0 0,
    MonthlyDebtSummary (YearAndMonth 2023 01) P1OwesP2 9300 4350
  ]

bigConf =
  ParseConf
    { colSep = ",",
      dateColNum = 0,
      amountColNum = 5,
      dataStartRow = 1
    }

-- These two sets of data are taken from a real CSV export from the Chase site,
-- with a bit of anonymizing of course
bigCsvData1 =
  "Transaction Date,Post Date,Description,Category,Type,Amount,Memo\n\
  \11/28/2021,11/28/2021,BILL'S FOOD BARN,Food & Drink,Sale,-57.15,\n\
  \11/22/2021,11/23/2021,BILL'S FOOD BARN,Food & Drink,Sale,-122.69,\n\
  \11/22/2021,11/23/2021,BILL'S FOOD BARN,Food & Drink,Sale,-47.49,\n\
  \11/21/2021,11/21/2021,BILL'S FOOD BARN,Food & Drink,Sale,-60.01,\n\
  \11/19/2021,11/19/2021,BILL'S FOOD BARN,Food & Drink,Sale,-71.44,\n\
  \11/14/2021,11/14/2021,BILL'S FOOD BARN,Food & Drink,Sale,-74.85,\n\
  \11/11/2021,11/11/2021,BILL'S FOOD BARN,Food & Drink,Sale,-92.65,\n\
  \11/06/2021,11/07/2021,BILL'S FOOD BARN,Food & Drink,Sale,-50.60,\n\
  \10/31/2021,11/01/2021,BILL'S FOOD BARN,Food & Drink,Sale,-90.23,\n\
  \12/30/2021,12/30/2021,BILL'S FOOD BARN,Food & Drink,Sale,-89.40,\n\
  \12/29/2021,12/29/2021,BILL'S FOOD BARN,Food & Drink,Sale,-56.57,\n\
  \12/28/2021,12/28/2021,BILL'S FOOD BARN,Food & Drink,Sale,-96.33,\n\
  \12/24/2021,12/24/2021,BILL'S FOOD BARN,Food & Drink,Sale,-44.84,\n\
  \12/23/2021,12/23/2021,BILL'S FOOD BARN,Food & Drink,Sale,-60.47,\n\
  \12/22/2021,12/22/2021,BILL'S FOOD BARN,Food & Drink,Sale,-65.60,\n\
  \12/20/2021,12/20/2021,BILL'S FOOD BARN,Food & Drink,Sale,-72.37,\n\
  \12/18/2021,12/19/2021,BILL'S FOOD BARN,Food & Drink,Sale,-39.91,\n\
  \12/12/2021,12/12/2021,BILL'S FOOD BARN,Food & Drink,Sale,-185.78,\n\
  \12/05/2021,12/05/2021,BILL'S FOOD BARN,Food & Drink,Sale,-90.62,\n\
  \12/02/2021,12/03/2021,BILL'S FOOD BARN,Food & Drink,Sale,-43.39,\n\
  \12/02/2021,12/02/2021,BILL'S FOOD BARN,Food & Drink,Sale,-81.18,\n\
  \12/01/2021,12/01/2021,BILL'S FOOD BARN,Food & Drink,Sale,-52.20,\n\
  \01/29/2022,01/30/2022,BILL'S FOOD BARN,Food & Drink,Sale,-56.68,\n\
  \01/28/2022,01/28/2022,BILL'S FOOD BARN,Food & Drink,Sale,-39.34,\n\
  \01/24/2022,01/24/2022,BILL'S FOOD BARN,Food & Drink,Sale,-78.17,\n\
  \01/23/2022,01/23/2022,BILL'S FOOD BARN,Food & Drink,Sale,-87.47,\n\
  \01/17/2022,01/18/2022,BILL'S FOOD BARN,Food & Drink,Sale,-129.74,\n\
  \01/15/2022,01/16/2022,BILL'S FOOD BARN,Food & Drink,Sale,-12.00,\n\
  \01/15/2022,01/16/2022,BILL'S FOOD BARN,Food & Drink,Sale,-256.27,\n\
  \01/13/2022,01/13/2022,BILL'S FOOD BARN,Food & Drink,Sale,-82.73,\n\
  \01/11/2022,01/11/2022,BILL'S FOOD BARN,Food & Drink,Sale,-90.08,\n\
  \01/09/2022,01/09/2022,BILL'S FOOD BARN,Food & Drink,Sale,-72.26,\n\
  \01/06/2022,01/06/2022,BILL'S FOOD BARN,Food & Drink,Sale,-58.20,\n\
  \01/04/2022,01/04/2022,BILL'S FOOD BARN,Food & Drink,Sale,-77.56,\n\
  \01/02/2022,01/02/2022,BILL'S FOOD BARN,Food & Drink,Sale,-56.48,\n\
  \01/01/2022,01/02/2022,BILL'S FOOD BARN,Food & Drink,Sale,-242.25,\n"

bigCsvData2 =
  "Transaction Date,Post Date,Description,Category,Type,Amount,Memo\n\
  \01/31/2022,01/31/2022,BILL'S FOOD BARN,Food & Drink,Sale,-75.88,\n\
  \01/26/2022,01/26/2022,BILL'S FOOD BARN,Food & Drink,Sale,-76.38,\n\
  \01/16/2022,01/16/2022,BILL'S FOOD BARN,Food & Drink,Sale,-52.30,\n\
  \01/14/2022,01/14/2022,BILL'S FOOD BARN,Food & Drink,Sale,-69.11,\n\
  \01/08/2022,01/09/2022,BILL'S FOOD BARN,Food & Drink,Sale,-52.05,\n\
  \01/08/2022,01/09/2022,BILL'S FOOD BARN,Food & Drink,Sale,-9.99,\n\
  \01/07/2022,01/07/2022,BILL'S FOOD BARN,Food & Drink,Sale,-63.80,\n\
  \01/05/2022,01/05/2022,BILL'S FOOD BARN,Food & Drink,Sale,-64.18,\n\
  \01/03/2022,01/03/2022,BILL'S FOOD BARN,Food & Drink,Sale,-66.92,\n\
  \12/18/2021,12/19/2021,BILL'S FOOD BARN,Food & Drink,Sale,-67.23,\n\
  \12/16/2021,12/16/2021,BILL'S FOOD BARN,Food & Drink,Sale,-92.92,\n\
  \12/13/2021,12/13/2021,BILL'S FOOD BARN,Food & Drink,Sale,-51.13,\n\
  \12/10/2021,12/10/2021,BILL'S FOOD BARN,Food & Drink,Sale,-44.37,\n\
  \12/08/2021,12/08/2021,BILL'S FOOD BARN,Food & Drink,Sale,-9.99,\n\
  \12/05/2021,12/06/2021,BILL'S FOOD BARN,Food & Drink,Sale,-104.09,\n\
  \11/29/2021,11/29/2021,BILL'S FOOD BARN,Food & Drink,Sale,-62.05,\n\
  \11/25/2021,11/25/2021,BILL'S FOOD BARN,Food & Drink,Sale,-68.23,\n\
  \11/23/2021,11/23/2021,BILL'S FOOD BARN,Food & Drink,Sale,-67.85,\n\
  \11/17/2021,11/17/2021,BILL'S FOOD BARN,Food & Drink,Sale,-49.89,\n\
  \11/15/2021,11/15/2021,BILL'S FOOD BARN,Food & Drink,Sale,-93.21,\n\
  \11/12/2021,11/12/2021,BILL'S FOOD BARN,Food & Drink,Sale,-85.52,\n\
  \11/09/2021,11/09/2021,BILL'S FOOD BARN,Food & Drink,Sale,-89.52,\n\
  \11/08/2021,11/08/2021,BILL'S FOOD BARN,Food & Drink,Sale,-9.99,\n\
  \11/07/2021,11/08/2021,BILL'S FOOD BARN,Food & Drink,Sale,-65.15,\n"

-- TODO: round off before returning summary! 2 decimals
bigSummaries =
  [ MonthlyDebtSummary (YearAndMonth 2021 10) P2OwesP1 90.23 45.115,
    MonthlyDebtSummary (YearAndMonth 2021 11) P1OwesP2 1168.29 7.264999999999986,
    MonthlyDebtSummary (YearAndMonth 2021 12) P2OwesP1 1348.39 304.46500000000003,
    MonthlyDebtSummary (YearAndMonth 2022 1) P2OwesP1 1869.8400000000001 404.31000000000006
  ]
