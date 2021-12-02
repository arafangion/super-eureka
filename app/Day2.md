# Day 2, Part 1

Again, lets get the imports out of the way...

```haskell
module Day2 where

import qualified Data.List.Split

```

We have example data.  Again, lets be lazy and write it explicity:

```haskell
exampleLines = [ "forward 5"
               , "down 5"
               , "forward 8"
               , "up 3"
               , "down 8"
               , "forward 2"
               ]
```

We should define enumerations and all that fancy stuff to represent the data, but this is Advent of Code and generally lets just do the simplest thing that works, so lets not bother with that, but lets parse it slightly:

We have words (directions), and numbers - and it looks like we will be adding and doing some very basic maths on them, so lets make them numbers:

```haskell
parseLine :: String -> ( String, Int ) 
parseLine line = case Data.List.Split.splitOn " " line of
                  (direction:amount:_)-> (direction, read amount)

exampleData :: [ String ] -> [ (String, Int) ]
exampleData lines = map parseLine lines
```

That gives the following when we test it in `cabal repl`:

```
ghci> exampleData exampleLines
[("forward",5),("down",5),("forward",8),("up",3),("down",8),("forward",2)]
```

Sweet. Lets write our interpreter!

This interpreter requires the commands (a direction string, and a amount Int), a starting point (position and depth), and produces the final point (position and depth)

```haskell

interpret :: [ (String, Int) ] -> (Int, Int) -> (Int, Int)
interpret ((direction,amount):xs) (position,depth) =
  interpret
    xs 
    (
      case direction of
        "forward" -> (position + amount, depth)
        "up" -> (position, depth - amount)
        "down" -> (position, depth + amount)
    )
interpret [] result = result
```

Testing that in the `cabal repl` gives us:

```
ghci> interpret (exampleData exampleLines) (0, 0)
(15,10)
```

Sweet! :)

Now for the real data... Which I've saved in `data/day2p1`:

```haskell
day2p1 = do
  inputData <- lines <$> readFile "data/day2p1"

  let (pos, depth) = interpret (exampleData inputData) (0, 0)

  putStrLn $ "Day 2 Part 1 results in: " <> show (pos * depth)

```

# Tying it together:

```haskell
day2 = do
  day2p1
  putStrLn "Day 2...  TBD"
```
