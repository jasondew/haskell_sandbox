module EuclidTest where

import Test.QuickCheck
import Control.Monad
import Prelude hiding (gcd)
import Euclid

prop_GCDwithZero x = gcd x 0 == x
prop_GCDwithOne x = gcd x 1 == 1
prop_GCDwithMultiple x = gcd x (3 * x) == x

main :: IO ()
main = mapM_ quickCheck [prop_GCDwithZero, prop_GCDwithOne, prop_GCDwithMultiple]
