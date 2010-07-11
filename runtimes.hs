-- calculating the largest value of n such that an algorithm
-- takes f(n) microseconds to solve it

import Data.List
import Text.Printf
import Numeric

tolerance :: Double
tolerance = 1e-2

e :: Double
e = 2.718281828

times :: [Double]
times = [100, 100*60, 100*60*60, 100*60*60*24, 100*60*60*24*30, 100*60*60*24*30*12, 100*60*60*24*30*12*100]

inverseOfNLgN :: Double -> Double
inverseOfNLgN x = x / lambertW(x)

factorials = scanl (*) 1 [1..]
factorial = (factorials !!)

inverseOfNFactorial :: Double -> Double
inverseOfNFactorial x = fromIntegral $ f 1 x
  where f i x | (factorial i) > x = i - 1
              | otherwise         = f (i + 1) x

lambertW :: Double -> Double
lambertW x = recursiveLambertW ((log x) - (log (log x))) x
  where recursiveLambertW previous x = case abs (previous - new) < tolerance of
                                         True  -> new
                                         False -> recursiveLambertW new x
                                       where new = previous - (previous * ew - x) / (ew + previous * ew)
                                             ew  = e ** previous

inverse_functions :: [(String, Double -> Double)]
inverse_functions = [("ln(n)", \x -> e ** x), ("sqrt(n)", (** 2)), ("n", id), ("n ln(n)", inverseOfNLgN),
                     ("n^2", (** 0.5)), ("n^3", (** 0.3333)), ("2^n", \x -> (log x) / (log 2)), ("n!", inverseOfNFactorial)]

main :: IO ()
main = do
  putStrLn $ "<table><thead><tr>" ++
             "<th>complexity</th>" ++
             "<th>1 second</th>" ++
             "<th>1 minute</th>" ++
             "<th>1 hour</th>" ++
             "<th>1 day</th>" ++
             "<th>1 month</th>" ++
             "<th>1 year</th>" ++
             "<th>1 century</th>" ++
             "</tr></thead><tbody>"
  mapM_ printRow inverse_functions
  putStrLn "</tbody></table>"

  where printRow (name, f) = do
          putStr "<tr>"
          mapM_ printCell $
            [name] ++ map format [f t | t <- times]
          putStrLn "</tr>"
        format x =
          showEFloat (Just 2)
                     (fromIntegral $ floor x)
                     []
        printCell s =
          putStr $ "<td>" ++ s ++ "</td>"
