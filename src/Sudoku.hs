-- |
-- Module      : Sudoku
-- Description : A Sudoku solver.
-- Maintainer  : jimburton1@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- A Sudoku solver.
module Sudoku  where

import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, elemIndex)
import Data.List.Split (chunksOf)

-- * Solving Sudoku puzzles

-- | A Sudoku puzzle is a list of lists, where each value is a Maybe Int. That is,
-- | each value is either `Nothing' or `Just n', for some Int value `n'.
newtype Puzzle = Puzzle [[Maybe Int]]
 deriving (Show, Eq)

-- | A Block is a list of 9 Maybe Int values. Each Block represents a row, a column,
-- or a 3x3 square.
type Block = [Maybe Int]

-- | A zero-based (row, column) position within the puzzle.
newtype Pos = Pos (Int, Int) deriving (Show, Eq)

-- | A getter for the rows in a Sudoku puzzle.
rows :: Puzzle -> [[Maybe Int]]
rows (Puzzle rs) = rs

-- | An example puzzle.
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

-- | Ex 1.1: Create a puzzle with just blanks.
allBlankPuzzle :: Puzzle
allBlankPuzzle = undefined

-- | Ex 1.2: Return True if @sud@ is really a valid representation of a sudoku
-- | puzzle (9 rows and columns, every value is either Nothing or (Just i) where
-- | i is between 1 and 9).
isPuzzle :: Puzzle -> Bool
isPuzzle = undefined

-- | Ex 1.3: Return True if the puzzle is solved, i.e. there are no blanks.
isSolved :: Puzzle -> Bool
isSolved = undefined

-- | Ex 2.1: @printPuzzle p@ prints a representation of @p@.
printPuzzle :: Puzzle -> IO ()
printPuzzle = undefined

-- | Ex 2.2: @readPuzzle f@ reads from the @FilePath@ @f@, and constructs
-- | a Puzzle from the contents.
readPuzzle :: FilePath -> IO Puzzle
readPuzzle = undefined

-- | Ex 3.1: Return True if a block contains valid and not duplicated values.
isValidBlock :: Block -> Bool
isValidBlock = undefined

-- | Ex 3.2: Collect all blocks on a board - each row and column is a block,
-- | as are the 9 3x3 squares.
blocks :: Puzzle -> [Block]
blocks = undefined

-- | Ex 3.3: Check that all blocks in a puzzle are valid.
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle = undefined

-- | Ex 4.1: Given a Puzzle that has not yet been solved, returns a position in
-- | the Puzzle that is still blank. If there is more than one blank
-- | position, you may decide yourself which one to return.
blank :: Puzzle -> Pos
blank = undefined

-- | Ex 4.2: Given a list, and a tuple containing an index in the list and a
-- | new value, updates the given list with the new value at the given index.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) = undefined

-- | Ex 4.3: @update s p v@ returns a puzzle which is a copy of @s@ except that
-- | the position @p@ is updated with the value @v@.
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update = undefined

-- | Ex 5.1: Solve the puzzle.
solve :: Puzzle -> Maybe Puzzle
solve = undefined

-- | Ex 5.2: Read a puzzle and return the solution, if it exists.
readAndSolve :: FilePath -> IO (Maybe Puzzle)
readAndSolve = undefined

-- | Ex 5.3: Return True if @s1@ is a solution of @s2@.
isSolutionOf :: Puzzle -> Puzzle -> Bool
isSolutionOf = undefined
