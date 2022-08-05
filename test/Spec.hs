import qualified ExpenseSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Expense" ExpenseSpec.spec
