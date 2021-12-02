# Day 1, part 1 - How many measurements are larger than the previous one?

First, lets get the imports out of the way:

```haskell
module Day1 where

import qualified Data.List
```

Lets load the sample data, here it's so short, lets load it by hand:

```haskell
exampleData = words "199 200 208 210 200 207 240 269 260 263"

dataMeasurements :: [ String ] -> [ Int ]
dataMeasurements = map read
```

From the example data, lets determine how many times it was increased.

First, lets pair everything up:

```haskell
pairwise :: [ Int ] -> [ (Int, Int) ]
pairwise x = zip x (tail x)
```

That should produce results similar to the following

```
> pairwise [1, 2, 3]
[(1,2),(2,3)]
```

Finally, we can foldr over all that, and accumulate the number of times the values went up!

See [the wiki about foldr](https://wiki.haskell.org/Fold)

```haskell
increases :: [ Int ] -> Int
increases measurements = 
  let 
    pairs :: [ (Int, Int) ]
    pairs = pairwise measurements
    f (prev, next) acc = if next > prev
                         then acc + 1
                         else acc
  in
    foldr f 0 pairs
```

Trying that in the `cabal repl` gives us:

```
> increases $ dataMeasurements exampleData
7
```

Excellent!  So lets try loading the real data...

```haskell
dataDay1Part1 = readFile "data/day1p1"
```

And lets wire it up so that we can see this result when we run `cabal run`...

```haskell
day1p1 :: IO ()
day1p1 = do
  part1 <- dataDay1Part1
  let
    part1Measurements = dataMeasurements $ lines part1
    part1result = increases part1Measurements

  putStrLn $ "Day 1 part 1 results in: " <> show part1result
```


# Day 1, part 2 - Turns out we need to use a sliding window instead of just two comparisons!

The main difference would be the `increases` function, which now needs to take into account a sliding window instead of just dealing with it all in a pairwise fashion.

It's almost right, but we need to change one part of it:

```
increasesP2 :: [ Int ] -> Int
increasesP2 measurements = 
  let 
    pairs :: [ (Int, Int) ]
    pairs = ... To be defined ...
    f (prev, next) acc = if next > prev
                         then acc + 1
                         else acc
  in
    foldr f 0 pairs
```

In order to impelment that bit, we need to be able to define a sliding window given a list, given a sliding window length, and the list data.

Here, I am applying a sliding window by duplicating the measurement lists, offsetting them by dropping the first few items appropriately and then zipping them up again.

*Note: Other people have a far simpler and better implementation of sliding windows. See the Discussion*

```haskell

sliding :: Int -> [Int] -> [[Int]]
sliding 1 measurements = map (:[]) measurements
sliding x measurements =
  let
    enumeratedLists :: [ (Int, [Int]) ]
    enumeratedLists = zip [0..] $ replicate x measurements

    offsetted :: [ [Int] ]
    offsetted = map (\(x, lst) -> drop x lst) enumeratedLists

    transposed = Data.List.transpose offsetted
    -- The windowing function produces shorter windows towards the end, so discard those
    filtered = filter (\lst -> length lst == x) transposed
  in
    filtered
```

There is probably a much shorter or more efficient way to implement the above, however, lets get it out of the way by implementing increasesP2:

```haskell
increasesP2 :: [ Int ] -> Int
increasesP2 measurements = 
  let 
    pairs :: [ (Int, Int) ]
    pairs = pairwise $ map sum (sliding 3 measurements)
    f (prev, next) acc = if next > prev
                         then acc + 1
                         else acc
  in
    foldr f 0 pairs
```

Lets try this in the `cabal repl` again:
```
> increasesP2 $ dataMeasurements exampleData
5
```

If we got 5 for the sample data, that's excellent. Lets wire it up for `cabal run`:

```haskell
day1p2 :: IO ()
day1p2 = do
  part1 <- dataDay1Part1
  let
    part1Measurements = dataMeasurements $ lines part1
    part1result = increasesP2 part1Measurements

  putStrLn $ "Day 1 part 2 results in: " <> show part1result
```

# Discussion

After looking at other solutions and discussing with people, it turns out that there's a very obvious way to implement the sliding function:

```haskell
slidingR1 measurements = take 3 <$> Data.List.tails measurements

increasesP2R1 :: [ Int ] -> Int
increasesP2R1 measurements = 
  let 
    pairs :: [ (Int, Int) ]
    pairs = pairwise $ map sum (slidingR1 measurements)
    f (prev, next) acc = if next > prev
                         then acc + 1
                         else acc
  in
    foldr f 0 pairs

day1p2r1 :: IO ()
day1p2r1 = do
  part1 <- dataDay1Part1
  let
    part1Measurements = dataMeasurements $ lines part1
    part1result = increasesP2 part1Measurements

  putStrLn $ "Day 1 part 2 results in: " <> show part1result <> " <-- Revision 1"
```

I've also had merijn show his sliding window:
```
slidingWindow :: (Ord a, Num a) => [a] -> [a]
slidingWindow (x1:xs@(x2:x3:_)) = x1 + x2 + x3 : slidingWindow xs
slidingWindow _ = []
```

That one is my favorite, because:
* It's short.
* It does exactly what's required: Produce a list containing the sums of each sliding window, 3 at a time.
* And it's not overly flexible in ways that aren't yet needed!  We don't need to have variable sliding windows, so why bother making the function generic?


And that's my favorite solution 

# Bringing it together for main:

```haskell
day1 = do
  day1p1
  day1p2
  day1p2r1

```
