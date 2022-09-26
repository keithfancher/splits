import qualified ExpenseSpec
import qualified ParseSpec
import qualified ProcessSpec
import qualified SummarySpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Expense" ExpenseSpec.spec
  describe "Parse" ParseSpec.spec
  describe "Process" ProcessSpec.spec
  describe "Summary" SummarySpec.spec
