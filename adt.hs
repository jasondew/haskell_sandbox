data IntSet = IntSet [Int] deriving (Show)

myIntSet :: IntSet
myIntSet = IntSet [1, 2, 3]

pseudoIntSet :: [Int]
pseudoIntSet = [1,2,3]

union :: IntSet -> IntSet -> IntSet
union (IntSet a) (IntSet b) = IntSet $ a ++ b

main = do
-- would not work, name equivalence not type equivalence
--  putStrLn $ show $ myIntSet `union` pseudoIntSet
  putStrLn $ show $ myIntSet `union` myIntSet
