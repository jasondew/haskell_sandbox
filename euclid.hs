module Euclid where

import Prelude hiding (gcd)

gcd :: Int -> Int -> Int
gcd x 0 = x
gcd 0 y = y
gcd x y | x > y     = gcd y (x `mod` y)
        | otherwise = gcd x (y `mod` x)

gcd' :: Int -> Int -> Int
gcd' x 0 = x
gcd' 0 y = y
gcd' x y | x > y      = gcd' (x - y) y
         | otherwise  = gcd' x (y - x)
