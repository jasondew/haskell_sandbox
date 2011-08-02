chain :: Integer -> [Integer]
chain n
  | n == 1 = [1]
  | even n = n : chain (n `div` 2)
  | odd n  = n : chain (3 * n + 1)

chainLengths :: [Int]
chainLengths = map (length . chain) [1..]
