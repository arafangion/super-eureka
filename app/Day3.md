# Day 3, part 1

Just a quick one today, this is Friday evening, afterall.


```haskell
module Day3 where

import Data.List
import Numeric
import Data.Char

exampleData = words "00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010"
mostCommon lst = map (\x -> (sort x) !! (length lst `div` 2)) $ transpose lst
leastCommon lst = map (\x -> case x of
                        '0' -> '1'
                        '1' -> '0'
                      ) $ mostCommon lst

fromBinary :: String -> Int
fromBinary binStr = case (readInt 2 (\x -> x == '1' || x == '0') digitToInt) binStr of
                      [(result,_)] -> result

rating bc _ [x] = x
rating bc pos lst = rating bc (pos + 1) $ filter (bc lst pos) lst

oxygen lst pos bits =
  let
    bit = mostCommon lst !! pos
  in
    (bits !! pos) == bit
  
scrubber lst pos bits =
  let
    bit = leastCommon lst !! pos
  in
    (bits !! pos) == bit

day3 = do
  input <- lines <$> readFile "data/day3p1"
  let
    most = fromBinary $ mostCommon input
    least = fromBinary $ leastCommon input
  
  putStrLn $ "Day 3 part 1 is: " <> (show $ most * least)

  let
    o2 = fromBinary $ rating oxygen 0 input
    co2 = fromBinary $ rating scrubber 0 input
  putStrLn $ "Day 3 part 2 is: " <> (show $ o2 * co2)

```

