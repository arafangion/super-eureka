# Day 3, part 1

Just a quick one today, this is Friday evening, afterall.


First, lets get the improts out of the way...
```haskell
module Day3 where

import Data.List
import Numeric
import Data.Char
```

First thing we must do, is have an example!  We define `exampleData`, giving it the Day3 example, we pass it through the `words` function which splits it up by whitespace and results in a list of strings.
```haskell
exampleData = words "00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010"
```

The problem wants us to determine the 'most common' bits for each column - so lets first transpose the data so that is easier to work with.
In python, this would basically be a `zip(*lst)`, but here we do `transpose lst`

This gives us a list of all the bits for a given column, eg, the first item is `011110011100`, we sort these characters and fetch the middle one: For a sorted set of characters, this would effectively be the most common 'bit' for the original input column.
```haskell
mostCommon lst = map (\x -> (sort x) !! (length lst `div` 2)) $ transpose lst
```

For 'leastCommon', I cheat: I just flip the mostCommon bits around:
```haskell
leastCommon lst = map (\x -> case x of
                        '0' -> '1'
                        '1' -> '0'
                      ) $ mostCommon lst
```

Haskell doesn't actually have a generic 'string to integer with a given base' function, so here, we use a "parser", the end result is 'fromBinary' will convert the zeros and ones to an actual integer value.
```haskell
fromBinary :: String -> Int
fromBinary binStr = case (readInt 2 (\x -> x == '1' || x == '0') digitToInt) binStr of
                      [(result,_)] -> result
```

# Day 3, Part 2

It turns out we have to do a bit more calculations based on oxygen and CO2 scrubber ratios or something.

First, we define a 'rating' function that recursively calls itself with the new list; First defining the base case - a list consisting of only one item - in this case, we provide just that item.
Then, we provide the recursive pattern - it recursively calls itself with the new list that has been filtered according to the given 'bit criteria' `bc`, which reduces the list each time.

Obviously, the AoC data set is going to be somehow carefully curated so that we have only the one result after all of this...

```haskell
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
```

# Putting it together

```haskell
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

