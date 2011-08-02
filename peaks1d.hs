-- finding the "peaks" in a 1d array

import System(getArgs)
import Test.HUnit

tests = test ["test1" ~: "edge case"      ~: []    ~=? (peaks1d []),
              "test2" ~: "trivial case"   ~: [5]   ~=? (peaks1d [5]),
              "test3" ~: "single peak"    ~: [4]   ~=? (peaks1d [1,4,2]),
              "test4" ~: "multiple peaks" ~: [5,4] ~=? (peaks1d [1,4,2,5,3]),
              "test5" ~: "boundary peak"  ~: [5]   ~=? (peaks1d [5,4,3])]

-- O(n) version
peaks1d :: [Integer] -> [Integer]
peaks1d [] = []
peaks1d xs = f [] $ [-1] ++ xs ++ [-1]   -- padding to catch the boundary case, assume elements >= 0
  where f peaks xs | (length xs < 3) = peaks
        f peaks (x:y:z:rest)         = case (y > x) && (y > z) of
                                         True  -> f (y:peaks) (y:z:rest)
                                         False -> f peaks (y:z:rest)

peaks1d_fast :: [Integer] -> [Integer]

-- run with no argument to run the tests
-- run with an array to find the peaks
-- ex: runghc peaks1d.hs [1,2,4,2,5,3,8]
main = do
  args <- getArgs
  case (length args == 0) of
    True  -> (runTestTT tests) >> (putStrLn "")
    False -> putStrLn . show . peaks1d . read . head $ args
