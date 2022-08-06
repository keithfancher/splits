import qualified ExpenseSpec
import qualified PersonSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Expense" ExpenseSpec.spec
  describe "Person" PersonSpec.spec
