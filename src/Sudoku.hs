module Sudoku where

import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, elemIndex)
import Data.List.Split (chunksOf)
import Control.Monad (liftM, replicateM_)
import Test.QuickCheck

-------------------------------------------------------------------------

{-| A Sudoku puzzle is a list of lists, where each value is a Maybe Int. That is,
each value is either `Nothing' or `Just n', for some Int value `n'. |-}
data Puzzle = Puzzle [[Maybe Int]]
 deriving (Show, Eq)

{-| A Block is a list of 9 Maybe Int values. Each Block represents a row, a column,
or a square. |-}
type Block = [Maybe Int]

{-| A Pos is a zero-based (row, column) position within the puzzle. |-}
type Pos = (Int, Int) 

{-| A getter for the rows in a Sudoku puzzle. |-}
rows :: Puzzle -> [[Maybe Int]]
rows = undefined

{-| A Sudoku puzzle with just blanks. |-}
allBlankPuzzle :: Puzzle
allBlankPuzzle = undefined

{-| Checks if `s' is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
isPuzzle s = undefined

{-| Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isFull :: Puzzle -> Bool
isFull s = undefined

{-| Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
isValidBlock b = undefined

{-| Collect all blocks on a board - the rows, the columns and the squares. |-}
blocks :: Puzzle -> [Block]
blocks s = undefined

{-| Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle s = undefined

{-| Check whether a puzzle is solved. |-}
isSolved :: Puzzle -> Bool
isSolved s = undefined

{-| Given a Puzzle that has not yet been solved, returns a position in the Puzzle that is still blank. If there are more than one blank position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
blank s = undefined

{-| Given a list, and a tuple containing an index in the list and a new value, updates the given list with the new value at the given index. |-}
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (i, y) = undefined

{-| `update s p v' returns a puzzle which is a copy of `s' except that the position
`p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update s p v = undefined

{-| Solve the puzzle. |-}
solve :: Puzzle -> Maybe Puzzle
solve s = undefined

{-| `printPuzzle s' prints a representation of `s'. |-}
printPuzzle :: Puzzle -> IO ()
printPuzzle s = undefined

{-| `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
if `f' did not contain a puzzle. |-}
readPuzzle :: FilePath -> IO Puzzle
readPuzzle f = undefined

-------------------------------------------------------------------------
-- QuickCheck definitions and tests

{-| cell generates an arbitrary cell in a Puzzle with
    a 1 in 10 chance of being non-empty. |-}
cell :: Gen (Maybe Int)
cell = frequency [ (9, return Nothing)
                 , (1, do n <- choose(1,9) ; return (Just n))]

-- an instance for generating Arbitrary Puzzles
instance Arbitrary Puzzle where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Puzzle rows)

prop_allBlank :: Bool
prop_allBlank = let rs = rows allBlankPuzzle
                in
                 length rs == 9
                 && and (map ((==9) . length) rs)
                 && and ((concatMap (map isNothing)) rs)

prop_blocks :: Puzzle -> Bool
prop_blocks s = ((length bl) == 3*9) && 
                and [(length b) == 9 | b <- bl] where
                    bl = blocks s

-- (!!=): The length should be the same after replacing an element
prop_replaceoplength :: [a] -> (Int, a) -> Bool
prop_replaceoplength s (i,x) = length s == length (s !!= (i, x))

-------------------------------------------------------------------------
