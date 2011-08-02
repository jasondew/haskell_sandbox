import System(getArgs)
import Test.HUnit
import List(insert)

tests = test ["test1" ~: "edge case"    ~:            [] ~=? (sort []),
              "test2" ~: "trivial case" ~:           [5] ~=? (sort [5]),
              "test3" ~: "odd case"     ~:   [1,2,3,4,5] ~=? (sort [1,4,2,5,3]),
              "test4" ~: "even case"    ~: [1,2,3,4,5,6] ~=? (sort [6,4,2,5,3,1])]

insertionSort :: [Integer] -> [Integer]
insertionSort = f []
 where f sorted     [] = sorted
       f sorted (x:xs) = f (insert x sorted) xs

mergeSort :: [Integer] -> [Integer]
mergeSort  [] = []
mergeSort [x] = [x]
mergeSort  xs = merge [] (mergeSort xs1) (mergeSort xs2)
  where (xs1, xs2) = splitAt (length xs `div` 2) xs
        merge sorted     []     [] = sorted
        merge sorted     []     ys = sorted ++ ys
        merge sorted     xs     [] = sorted ++ xs
        merge sorted (x:xs) (y:ys) = case (x < y) of
                                       True  -> merge (sorted ++ [x]) xs (y:ys)
                                       False -> merge (sorted ++ [y]) (x:xs) ys

data Heap = Empty | Heap Integer Heap Heap

heapify :: [Integer] -> Heap
heapify     [] = Empty
heapify    [x] = Heap x Empty Empty
heapify (x:xs) = f Empty xs

heapSort :: [Integer] -> [Integer]

sort = heapSort

-- run with no argument to run the tests
-- run with an array to sort
-- ex: runghc sorts.hs [1,2,4,2,5,3,8]
main = do
  args <- getArgs
  case (length args == 0) of
    True  -> (runTestTT tests) >> (putStrLn "")
    False -> putStrLn . show . sort . read . head $ args
