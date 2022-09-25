import qualified ExpenseSpec
import qualified SummarySpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Expense" ExpenseSpec.spec
  describe "Summary" SummarySpec.spec
