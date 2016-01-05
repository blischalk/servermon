module Monitor.Test where
import Test.HUnit ((@=?), Test(..))
import Monitor


bogusTests :: [Test]
bogusTests = map TestCase
             [ 1 @=? 1]
