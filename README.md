# CI505 Assignment: Sudoku

Adapted with permission from http://www.cse.chalmers.se/edu/course/TDA555.

In this assignment you will design a Haskell program that will be
used to solve *Sudoku*, the well-known logical puzzle from Japan.

First, download a copy of this repository. You will do your work in
the file `src/Sudoku.hs` and the repository also includes other useful
material, such as example puzzles in the `puzzles/` directory. 

```
$ git clone https://github.com/jimburton/sudoku.git
$ cd sudoku
sudoku$ 
```
This is a Cabal project. Use `cabal` to compile the code and run the tests:

```
sudoku$ cabal configure
sudoku$ cabal run test-sudoku
--- output of tests
```

The easiest way to experiment with the code whilst you work on it is 
by running `cabal repl` from the top level of the project:

```
$ cd sudoku 
$ cabal repl
```

This runs `ghci` with all the right dependencies loaded. Now you can make changes to the
code in an editor and enter `:r` in the REPL to reload the program and test your work.

**To avoid being accused of plagiarism, do not make your solutions available online. 
If you use github or any similar service, make sure the repository is private.**

## Hints

Some assignments have **hints**. Often, these involve particular standard
Haskell functions that you could use. Some of these functions are
defined in modules that you have to import yourself explicitly. You
can use the following resources to find more information about those
functions:

+ [Hoogle, the library function search engine.](http://haskell.org/hoogle/)
+ [Haskell Library Structure](http://www.haskell.org/ghc/docs/latest/html/libraries/index.html): all standard libraries, for you to browse.
+ [A Tour of the Haskell Prelude](http://undergraduate.csse.uwa.edu.au/units/CITS3211/lectureNotes/tourofprelude.html): shows all standard Haskell functions
that you get without importing any module.

We encourage you to go and find information about the functions that
are mentioned in the hints.

## Preparation

Before starting on this assignment make sure that you have understood the
material from the lectures in week 1 to 4. It is not necessary to use
higher-order functions to solve the lab, but there are some tasks in
which certain higher-order functions may come in handy.

You should also be familiar with the `Maybe` type.

Submit the single file `Sudoku.hs` (not a copy of the whole
repository) on studentcentral before the deadline.

### Some general hints:

In order to "do something" with each row in a puzzle, you can use a
list comprehension:

```haskell
[... | r <- rows sud]
```
To operate on the columns instead, use the function

```haskell
transpose :: [[a]] -> [[a]]  -- import Data.List
```
to turn the list of rows into a list of columns.

An alternative to list comprehension is to use the higher-order function

```haskell
map :: (a -> b) -> [a] -> [b]
```

There are two ways to check that all elements in a puzzle satisfy some property:
Use list comprehension and one of the functions

```haskell
and :: [Bool] -> Bool  -- All elements are true?
or  :: [Bool] -> Bool  -- Some element is true?
```

Use one of the higher-order functions

```haskell
all :: (a -> Bool) -> [a] -> Bool  -- All elements satisfy the property?
any :: (a -> Bool) -> [a] -> Bool  -- Some element satisfies the property?
```

Try to minimize the use of IO instructions and do as much work as
possible using pure functions. For example, this program

```haskell
prog = putStr $ unlines ["line1", "line2"]
```
is better than

```haskell
prog = do
    putStrLn "line1"
    putStrLn "line2"
```

## Sudoku

Sudoku is a logic puzzle from Japan which gained popularity in the
West during the 90s. Most newspapers now publish a Sudoku puzzle for
the readers to solve every day.

A Sudoku puzzle consists of a 9x9 grid. Some of the cells in the grid
have digits (from 1 to 9), others are blank. The objective of the
puzzle is to fill in the blank cells with digits from 1 to 9, in such
a way that every row, every column and every 3x3 block has exactly one
occurrence of each digit 1 to 9.

Here is an example of a Sudoku puzzle:

```
+---+---+---+---+---+---+---+---+---+
|   |   | 1 |   |   |   | 7 | 5 |   |
+---+---+---+---+---+---+---+---+---+
|   | 8 |   |   | 2 |   |   |   |   |
+---+---+---+---+---+---+---+---+---+
|   | 3 | 6 |   | 4 |   |   | 8 |   |
+---+---+---+---+---+---+---+---+---+
|   |   |   | 6 |   | 1 | 2 | 4 |   |
+---+---+---+---+---+---+---+---+---+
| 7 |   | 4 |   | 5 |   | 6 |   | 8 |
+---+---+---+---+---+---+---+---+---+
|   | 6 |   | 8 |   | 3 |   | 7 | 4 |
+---+---+---+---+---+---+---+---+---+
|   |   |   | 1 |   |   |   | 3 |   |
+---+---+---+---+---+---+---+---+---+
| 8 |   | 2 |   |   |   | 3 |   | 6 |
+---+---+---+---+---+---+---+---+---+
|   |   | 7 | 4 |   |   | 8 |   | 1 |
+---+---+---+---+---+---+---+---+---+


```


And here is the solution:

```
+---+---+---+---+---+---+---+---+---+
| 4 | 2 | 1 | 9 | 6 | 8 | 7 | 5 | 3 |
+---+---+---+---+---+---+---+---+---+
| 9 | 8 | 3 | 7 | 2 | 4 | 1 | 6 | 5 |
+---+---+---+---+---+---+---+---+---+
| 1 | 3 | 6 | 2 | 4 | 5 | 9 | 8 | 7 |
+---+---+---+---+---+---+---+---+---+
| 3 | 5 | 8 | 6 | 7 | 1 | 2 | 4 | 9 |
+---+---+---+---+---+---+---+---+---+
| 7 | 1 | 4 | 3 | 5 | 2 | 6 | 9 | 8 |
+---+---+---+---+---+---+---+---+---+
| 2 | 6 | 9 | 8 | 1 | 3 | 5 | 7 | 4 |
+---+---+---+---+---+---+---+---+---+
| 6 | 7 | 5 | 1 | 8 | 9 | 4 | 3 | 2 |
+---+---+---+---+---+---+---+---+---+
| 8 | 4 | 2 | 5 | 9 | 7 | 3 | 1 | 6 |
+---+---+---+---+---+---+---+---+---+
| 5 | 9 | 7 | 4 | 3 | 6 | 8 | 2 | 1 |
+---+---+---+---+---+---+---+---+---+
```

You will write a Haskell program that can read in a Sudoku puzzle and
solve it. If you want to read more about Sudoku, here are a few links:

+ [The Daily Sudoku](http://www.dailysudoku.com/sudoku/index.shtml) has examples and explanations.
+ [The Wikipedia page about Sudoku](http://en.wikipedia.org/wiki/Sudoku).
+ [sudoku.com.au](http://sudoku.com.au/) has Sudoku puzzles that you can solve online.
 

## Modelling Sudoku

To implement a Sudoku-solving program, we need to come up with a way
of modelling Sudoku puzzles. A Sudoku puzzle is a matrix of digits or
blanks. The natural way of modelling a matrix is as a list of
lists. The outer list represents all the rows, and the elements of the
list are the elements of each row. Digits or blanks can be represented
by using the Haskell `Maybe` type. Digits are simply represented by `Int`.

Summing up, a natural way to represent Sudoku puzzles is using the following
Haskell datatype:

```haskell
  data Puzzle = Puzzle [[Maybe Int]]
```

Since it is convenient to have a function that extracts the actual
rows from the puzzle, we say:

```haskell
  rows :: Puzzle -> [[Maybe Int]]
  rows (Puzzle rs) = rs
```
 
For example, the above Sudoku puzzle has the following representation in Haskell:

```haskell
  example :: Puzzle
  example =
    Puzzle
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]
```

Now, a number of assignments follow, leading you step-by-step
towards an implementation of a Sudoku-solver.

## Some Basic Sudoku Functions

To warm up, we start with a number of basic functions on Suduko puzzles.

### Part 1

**1.1** Implement a function

```haskell  
  allBlankPuzzle :: Puzzle
```

that represents a puzzle that only contains blank cells (this means
that no digits are present).  Do not use copy-and-paste programming
here. Your definition does not need to be longer than a few short
lines.

**1.2** The `Puzzle` type we have defined allows for more things than
Sudoku puzzles. For example, there is nothing in the type definition that
says that a puzzle has 9 rows and 9 columns, or that digits need to
lie between 1 and 9. 

Implement a function

```haskell
  isPuzzle :: Puzzle -> Bool
```

that checks if all such extra conditions are met by the given puzzle.

#### Examples:

```
  Sudoku> isPuzzle (Puzzle [])
  False
  Sudoku> isPuzzle allBlankPuzzle
  True
  Sudoku> isPuzzle example
  True
  Sudoku> isPuzzle (Puzzle (tail (rows example)))
  False
```

**1.3** Our job is to solve puzzles. So, it would be handy to know when a
puzzle is solved or not. We say that a puzzle is solved if there are
no blank cells left. Implement the following function:

```haskell
  isSolved :: Puzzle -> Bool
```

Note that we do not check here if the puzzle is a valid solution; we
will do this later. This means that any puzzle without blanks (even
puzzles with the same digit appearing twice in a row) is considered
solved by this function.  

**Hints**

The following standard Haskell functions might come in handy:

```haskell
replicate :: Int -> a -> [a]
length    :: [a] -> Int
and       :: [Bool] -> Bool
```

See also the general hints above.

### Reading and Printing Puzzles

Next, we need to have a way of representing Sudoku puzzles in a file. In that
way, our program can read puzzles from a file, and it is easy for us
to create and store several Sudoku puzzles.

The following is an example text-representation that we will use in
this assignment. It actually represents the example above.

```
36..712..
.5....18.
..92.47..
....13.28
4..5.2..9
27.46....
..53.89..
.83....6.
..769..43
```

There are 9 lines of text in this representation, each corresponding
to a row. Each line contains 9 characters. A digit 1 – 9 represents a
filled cell, and a full stop (.) represents a blank cell.

## Part 2

**2.1** Implement a function:

```haskell  
  printPuzzle :: Puzzle -> IO ()
```
that, given a puzzle, creates instructions to print the Sudoku on the
screen, using the format shown above.  

Example:
```
  Sudoku> printPuzzle allBlankPuzzle
  .........
  .........
  .........
  .........
  .........
  .........
  .........
  .........
  .........
  Sudoku> printPuzzle example
  36..712..
  .5....18.
  ..92.47..
  ....13.28
  4..5.2..9
  27.46....
  ..53.89..
  .83....6.
  ..769..43

```
**2.2** Implement a function:

```haskell
  readPuzzle :: FilePath -> IO Puzzle
```

that, given a filename, creates instructions that read the puzzle from
the file, and deliver it as the result of the instructions. You may
decide yourself what to do when the file does not contain a
representation of a puzzle.  

Examples:

```
  Sudoku> s <- readPuzzle "../puzzles/example.sud"
  Sudoku> printPuzzle s
  36..712..
  .5....18.
  ..92.47..
  ....13.28
  4..5.2..9
  27.46....
  ..53.89..
  .83....6.
  ..769..43
  Sudoku> readPuzzle "Sudoku.hs"
  Exception: Not a Sudoku puzzle!
```

(Note: In the above example, we make use of the fact that commands in
GHCi are part of an implicit do block. This allows us to bind results
of IO instructions, just like in do notation: `s <- readPuzzle ...`)

**Hints**

To implement the above, you will need to be able to convert between
characters (type `Char`) and digits/integers (type `Int`). The standard
functions `chr` and `ord` (import the module `Data.Char`) will come in handy
here. Think about the following problems:

+ Given a character representing a digit, for example `‘3’ :: Char`, how
do you compute the integer value `3`?
+ Given a digit represented as an integer, for example `3 :: Int`, how do
you compute the character `‘3’`?

The constant value ord ‘0’ will play a central role in all this. You
can also use the function `digitToInt`.

Here are some functions that might come in handy:

```haskell
chr        :: Int -> Char
ord        :: Char -> Int
digitToInt :: Char -> Int
putStr     :: String -> IO ()
putStrLn   :: String -> IO ()
sequence_  :: [IO a] -> IO ()
readFile   :: FilePath -> IO String
lines      :: String -> [String]
```

This repository contains some example Sudoku files that you can
download and use in the `puzzles/` directory. There are easy and hard
examples. The easy ones should all be solvable by your final program
within minutes; the hard ones will probably take a very long time
(unless you do the extra assignments and optimise your solver).
 

#### Rows, Columns and Blocks


Now, we are going to think about what actually constitutes a valid
solution of a puzzle. There are three constraints that a valid
solution has to fulfill:

+ No row can contain the same digit twice.
+ No column can contain the same digit twice.
+ No 3x3 block can contain the same digit twice.

This leads us to the definition of a block; a block is either a row or
a column or a 3x3 block. A block therefore contains 9 cells:

```haskell
  type Block = [Maybe Int]
```

We are going to define a function that checks if a puzzle is not
violating any of the above constraints, by checking that none of the
blocks violate those constraints.

## Part 3

**3.1** Implement a function:

```haskell
  isValidBlock :: Block -> Bool
```

that, given a block, checks if that block does not contain the same
digit twice. 

Examples:

```
  Sudoku> isValidBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
  True
  Sudoku> isValidBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
  False
```

**3.2** Implement a function:
```haskell
  blocks :: Puzzle -> [Block]
```
that, given a puzzle, creates a list of all blocks of that puzzle. This means:

+ 9 rows,
+ 9 columns,
+ 9 3x3 blocks.

**3.3** Now, implement a function:

```haskell
  isValidPuzzle :: Puzzle -> Bool
```
that, given a puzzle, checks that all rows, colums and 3x3 blocks do
not contain the same digit twice.  

Examples:

```
  Sudoku> isValidPuzzle allBlankPuzzle
  True
  Sudoku> do sud <- readPuzzle "example.sud"; print (isValid sud)
  True
```

**Test**: `prop_isValidPuzzle`.

**Hints**

Here are some functions that might come in handy:

```haskell
nub       :: Eq a => [a] -> [a]
transpose :: [[a]] -> [[a]]
take      :: Int -> [a] -> [a]
drop      :: Int -> [a] -> [a]
```

Note that some of the above functions only appear when you import
`Data.List`. See also the general hints above.

### Positions and Finding Blanks

We are getting closer to the final solving function. Let us start
thinking about how such a function would work.

Given a puzzle, if there are no blanks left in the Sudoku, we are
done. Otherwise, there is at least one blank cell that needs to be
filled in somehow. We are going to write functions to find and
manipulate such a blank cell.

It is quite natural to start to talk about positions. A position is a
coordinate that identifies a cell in the puzzle matrix. Here is a way of
modelling coordinates:

```haskell
  data Pos = Pos (Int,Int)
```

We use positions as indicating first the row and then the column. It
is common in programming languages to start counting at 0. Therefore,
the position that indicates the upper left corner is (0,0), and the
position indicating the lower right corner is (8,8). And, for example,
the position (3,5) denotes the 6th cell in the 4th row.

## Part 4

**4.1** Implement a function:
```haskell
  blank :: Puzzle -> Pos
```

that, given a puzzle that has not yet been solved, returns a position
in the puzzle that is still blank. If there are more than one blank
position, you may decide yourself which one to return.

Examples:
```
  Sudoku> blank allBlankPuzzle
  (0,0)
  Sudoku> blank example
  (0,2)
```

**4.2** Implement a function:

```haskell
  (!!=) :: [a] -> (Int,a) -> [a]
```

that, given a list, and a tuple containing an index in the list and a
new value, updates the given list with the new value at the given
index.

Examples:
```
  Sudoku> ["a","b","c","d"] !!= (1,"apa")
  ["a","apa","c","d"]
  Sudoku> ["p","qq","rrr"] !!= (0,"bepa")
  ["bepa","qq","rrr"]
```

**Test**: `prop_listReplaceOp`.

**4.3** Implement a function:

```haskell
  update :: Puzzle -> Pos -> Maybe Int -> Puzzle
```

that, given a puzzle, a position, and a new cell value, updates the
given puzzle at the given position with the new value.

Example:
```
  Sudoku> printPuzzle (update allBlankPuzzle (1,3) (Just 5))
  .........
  ...5.....
  .........
  .........
  .........
  .........
  .........
  .........
  .........
```

**Hints**

There is a standard function `(!!)` in Haskell for getting a specific
element from a list. It starts indexing at 0, so for example to get
the first element from a list `xs`, you can use `xs !! 0`.

We usually use the standard function zip to pair up elements in a list
with their corresponding index. Example:

```
  *Main> ["apa","bepa","cepa"] `zip` [1..3]
  [("apa",1),("bepa",2),("cepa",3)]
```

This, in combination with list comprehensions, should be very useful
for this assignment.

Here are some more useful functions:

```haskell
head :: [a] -> a
(!!) :: [a] -> Int -> a
zip  :: [a] -> [b] -> [(a,b)]
```

You might want to take a look at the exercises and answers on lists
and list comprehensions.

## Solving puzzles

Finally, we have all the bits in place to attack our main problem:
Solving a given puzzle.

Our objective is to define a Haskell function

```haskell
  solve :: Puzzle -> Maybe Puzzle
```

The idea is as follows. If we have a puzzle `s` that we would like to
solve, we give it to the function solve. This will produce one of two
results: 

`Nothing`, in which case it was impossible to solve the
puzzle. There is no solution.  

`Just s’`, in which case `s’` is a puzzle that represents a
solution. In other words, `s’` 

1. contains no blanks, 
2. has no blocks (rows, columns, 3x3 blocks) that contain the same digit twice,
and 
3. is an extension of the original puzzle (meaning that none of
the original digits have changed).  

How should `solve` be implemented? Here is one idea. When we try to
solve a given puzzle `s`, there exist three cases: 

`s` violates some of the constraints (in other words: some of its blocks contain the
same digit twice). In this case, the answer of `solve` must be `Nothing`.

`s` does not contain any blanks (in other words: `s` is a
solution). In this case, the answer of `solve` must be `Just s`.

Otherwise, `s` must contain at least one blank cell. In this case, we
try to recursively solve `s` 9 times; each time we update the blank
cell with a concrete digit from 1 to 9. The first recursive attempt
that succeeds is our answer. If none of the recursive attempts
succeed, we return `Nothing`.  This method of problem solving is called
backtracking.

Since backtracking is not covered in the module, we provide you with a
skeleton implementation of the solver. If you want more challenge,
don’t look at the code provided here.

The solver is a function

```haskell
solve :: Puzzle -> Maybe Puzzle
```

which receives a puzzle that may not be solved and, if possible,
returns a solved puzzle. The idea is that solve will either return
immediately – if there’s a violation or if the puzzle is already
solved – or it will call itself recursively according to the method
above:

```haskell
solve :: Puzzle -> Maybe Puzzle
solve s | ...       = Nothing  -- There's a violation in s
        | ...       = Just s   -- s is already solved
        | otherwise = pickASolution possibleSolutions
  where
    nineUpdatedSuds   = ... :: [Puzzle]
    possibleSolutions = [solve s' | s' <- nineUpdatedSuds]
```

The final case is the tricky one. First, we have the local definition
`nineUpdatedSuds`, which is a list of nine puzzles. They should all be
the same as the puzzle `s`, except that the first blank cell should be
updated from `Nothing` to a numeric value. Since the cell can be updated
in nine different ways, we get a list of nine new puzzles.

By recursively solving these nine puzzles, we get a list of nine
possible solutions (`possibleSolutions`). Now it’s just the task of the
helper function `pickASolution` to pick one of them (if there is one):

```haskell
pickASolution :: [Maybe Puzzle] -> Maybe Puzzle
pickASolution suds = ...
```

A key to understanding this definition of `solve` is to realize that
solve can only return `Nothing` or a completely solved puzzle, as seen
in the two base cases. This means that any `Just` value in
`possibleSolutions` has to be a complete solution.

Another key is to see that the recursion in solve must reach one of
the base cases sooner or later, because in every step we take away one
blank cell. Taking away a blank cell means making progress towards a
solution or a conflict.

## Part 5

**5.1** Implement a function:

```haskell
  solve :: Puzzle -> Maybe Puzzle
```
using the above idea.

Unless you’re up for a challenge you are recommended to use the
skeleton code from above and just fill in the ... parts.

Examples:
```
  Sudoku> printPuzzle (fromJust (solve allBlankPuzzle))
  123456789
  456789123
  789123456
  214365897
  365897214
  897214365
  531642978
  642978531
  978531642
  Sudoku> do s <- readPuzzle "example.sud"; printPuzzle (fromJust (solve s))
  364871295
  752936184
  819254736
  596713428
  431582679
  278469351
  645328917
  983147562
  127695843
  Sudoku> do s <- readPuzzle "../puzzles/impossible.sud"; print (solve sud)
  Nothing
```
(In the above examples, we use the standard function `fromJust` from the
library `Data.Maybe`.)

**5.2** For your own convenience, define a function:
```haskell
  readAndSolve :: FilePath -> IO ()
```

that produces instructions for reading the puzzle from the given file,
solving it, and printing the answer.

Examples:
```
  Sudoku> readAndSolve "../puzzles/example.sud"
  364871295
  752936184
  819254736
  596713428
  431582679
  278469351
  645328917
  983147562
  127695843
  Sudoku> readAndSolve "impossible.sud"
  (no solution)
```

**5.3** Implement a function:
```haskell
  isSolutionOf :: Puzzle -> Puzzle -> Bool
```

that checks, given two puzzles, whether the first one is a solution
(i.e. all blocks are okay, there are no blanks), and also whether the
first one is a solution of the second one (i.e. all digits in the
second puzzle are maintained in the first one).

Examples:
```
  Sudoku> fromJust (solve allBlankPuzzle) `isSolutionOf` allBlankPuzzle
  True
  Sudoku> allBlankPuzzle `isSolutionOf` allBlankPuzzle
  False
  Sudoku> fromJust (solve allBlankPuzzle) `isSolutionOf` example
  False
```

**Hints**

All the work we did in the assignments 1 to 4 should be used in order
to implement the function solve.

To implement the third, recursive, case of `solve`, you can use a list
comprehension that enumerates all possible values for the blank
cell. You can then use the standard function `listToMaybe` to turn the
result into something of type `Maybe`.

QuickChecking the property `prop_SolveSound` will probably take a long
time. *Be patient*. Alternatively, there are a number of things you can
do about this.

You can test on fewer examples (using the QuickCheck function
`quickCheckWith`). You can for example define:

```haskell
  fewerCheck prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop
```

And then write `fewerCheck prop_SolveSound` when you want to QuickCheck
the property.  You can also generate puzzles with a different
probability distribution. Try varying the amount of digits in an
arbitrary puzzle by fiddling with the frequencies in the `cell` function
and see what happens.

You can make your function solve go faster (see extra assignments *X* and *Y*).
It is okay if you do not find a completely satisfactory solution to this.

Here are some useful functions:

```haskell
fromJust    :: Maybe a -> a
listToMaybe :: [a] -> Maybe a
```

We have provided an example of an impossible puzzle in the file
`puzzles/impossible.sud`.
 

## Extra Assignments

**Assignments X.1 to X.3 are not assessed.** You can choose freely
whether to do 0, 1 or more of these. So, these are not obligatory, but
you will learn more if you do them.

There are no perfect, pre-defined answers here, but try to show your
thoughts and understanding in your answers.

**X.1** The solving method we have used in this lab assignment is very
basic, and in some sense naive. One way to boost performance is to
look closer at the function blank. Perhaps if we picked the blank in a
smarter way, the solve function would go faster?  One idea is to
always pick the blank spot where there are as few possibilities
left. For example, if we have a row with one or two blank spots, it is
probably a good idea to pick one of those blank spots, since it will
limit the consecutive search most, and it will lead to search to a
state with more digits filled in. (Such a way of changing a solving
method is called a heuristic – there is no absoluate guarantee that
the search will go faster, but often it actually will.)

Change the implementation of the blank function, so that it always
picks the blank spot that is in a row, column, or 3x3 block where
there are as few blank spots left.

For example, in the puzzle below:
```
  36..712..
  .5....18.
  ..92.47..
  ...x13.28
  4..5.2..9
  27.46....
  ..53.89..
  .83....6.
  ..769..43
```

we have marked one blank spot with an x. The row in which this x is
has 5 blank spots (including the x itself); the column in which this x
is has 4 blank spots, and the 3x3 block in which this x is has 3 blank
spots. It turns out that this is the best we can do; it is good to
pick x as the next blank spot, since there will only be 2 blank spots
left in the middle 3x3 block.

Does your solve function work faster now? Experiment with different
heuristics (for example: only look at rows and columns, and not at 3x3
blocks), and see which one performs best. Can you solve some of the
hard puzzles now?

Do not forget to add appropriate properties that test your functions.

**X.2** The solving method we have used in this lab assignment is very
basic, and in some sense naive. The best known methods to solve
problems like Sudoku is to also include the notion of
propagation. This is the way most humans actually solve a puzzle.

A simple variant of propagation is the following. Suppose we have
a puzzle containing a row with precisely one blank, such as the 3rd row in the
example below:
```
  36..712..
  .5....18.
  ..92.47..
  596.13428
  4..5.2..9
  27.46....
  ..53.89..
  .83....6.
  ..769..43
```

Our current solution would go and pick blanks, and start searching
recursively, without making use of the fact that we already know the
value of that blank (namely 7 in this case); all the other values have
been used by the other cells in the row.

Implement a function
```haskell
  propagate :: Puzzle -> Puzzle
```

that, given a puzzle, finds out which rows, columns, and 3x3 blocks
only have one blank in them, and then fills those blanks with the only
possibly remaining value. It repeats doing this until all rows,
columns and 3x3 blocks are either completely filled up, or contain two
holes or more.  Now, add this function at the appropriate place in
your solve function. Does it work faster now?

For other, more powerful propagation, you can for example read the following webpage:

sudoku.com, and click on "how to solve". Or come up with your own propagation rules.
Do not forget to add appropriate properties that test your functions.

**X.3** Write a function that produces interesting Sudoku puzzles. For
example, one could have a function

```haskell
  createPuzzle :: IO ()
```

that every time we run it, would print a new, interesting Sudoku
puzzle on the screen.  One can discuss what an interesting Sudoku
puzzle is. Here are three properties that an interesting Sudoku puzzle
must have:

+ There must be a solution
+ There must not be two different solutions
+ There must not be too many digits already visible

Can you think of a way to define a function for generating an infinite
supply of new Sudoku puzzles satisfying the above two properties? You
should of course make use of the functions you already have.

If you undertake the extensions, be sure to add appropriate properties
that test your new functions in the test suite, `tests/TestSudoku.hs`.

