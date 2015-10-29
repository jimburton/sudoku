{-# LANGUAGE TemplateHaskell #-}
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
data Pos = Pos (Int, Int) deriving (Show, Eq)

{-| A getter for the rows in a Sudoku puzzle. |-}
rows :: Puzzle -> [[Maybe Int]]
rows (Puzzle rs) = rs

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

{-| Ex 1.1

    A sudoku with just blanks. |-}
allBlankPuzzle :: Puzzle
allBlankPuzzle = undefined

{-| Ex 1.2

    Checks if sud is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
isPuzzle = undefined

{-| Ex 1.3

    Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isSolved :: Puzzle -> Bool
isSolved = undefined

{-| Ex 2.1

    `printPuzzle s' prints a representation of `s'. |-}
printPuzzle :: Puzzle -> IO ()
printPuzzle = undefined

{-| Ex 2.2

    `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
    if `f' did not contain a puzzle. |-}
readPuzzle :: FilePath -> IO Puzzle
readPuzzle = undefined

{-| Ex 3.1

    Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
isValidBlock = undefined

{-| Ex 3.2

    Collect all blocks on a board - the rows, the columns and the squares. |-}
blocks :: Puzzle -> [Block]
blocks = undefined

{-| Ex 3.3

    Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle = undefined

{-| Ex 4.1

    Given a Puzzle that has not yet been solved, returns a position in
    the Puzzle that is still blank. If there are more than one blank
    position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
blank = undefined

{-| Ex 4.2

    Given a list, and a tuple containing an index in the list and a
    new value, updates the given list with the new value at the given
    index. |-}
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) = undefined

{-| Ex 4.3

    `update s p v' returns a puzzle which is a copy of `s' except that
    the position `p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update = undefined

{-| Ex 5.1

    Solve the puzzle. |-}
solve :: Puzzle -> Maybe Puzzle
solve = undefined

{-| Ex 5.2

    Read a puzzle and solve it. |-}
readAndSolve :: FilePath -> IO (Maybe Puzzle)
readAndSolve = undefined

{-| Ex 5.3

    Checks if s1 is a solution of s2. |-}
isSolutionOf :: Puzzle -> Puzzle -> Bool
isSolutionOf = undefined

-------------------------------------------------------------------------
-- QuickCheck tests:
--
-- Run these in ghci, as
--
-- Sudoku> quickCheck prop_myProp
--
-- or
--
-- Sudoku> runTests
--
-- But note that some tests, prop_solve in particular, may take a long time to run.
-- You can run a test with fewer cases by running
--
-- > fewerCheck prop_solve
--
-- or optimise your solver so that it runs faster!
--
-- Feel free to add your own tests.
-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Puzzle
cell :: Gen (Maybe Int)
cell = frequency [ (9, return Nothing)
                 , (1, do n <- choose(1,9) ; return (Just n))]

-- an instance for generating Arbitrary Puzzles
instance Arbitrary Puzzle where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Puzzle rows)

instance Arbitrary Pos where
  arbitrary = do r <- choose (0,8)
                 c <- choose (0,8)
                 return $ Pos (r,c)

prop_allBlank :: Bool
prop_allBlank = let rs = rows allBlankPuzzle
                in
                 length rs == 9
                 && and (map ((==9) . length) rs)
                 && and ((concatMap (map isNothing)) rs)

prop_isPuzzle :: Puzzle -> Bool
prop_isPuzzle s = isPuzzle s

prop_isNotPuzzle :: Bool
prop_isNotPuzzle = not $ isPuzzle (Puzzle [[]])

prop_blocks :: Puzzle -> Bool
prop_blocks s = ((length bl) == 3*9) && 
                and [(length b) == 9 | b <- bl]
  where bl = blocks s

prop_isValidPuzzle :: Puzzle -> Bool
prop_isValidPuzzle s = isValidPuzzle s || not (null bads)
  where bads = filter (not . isValidBlock) (blocks s)

prop_blank :: Puzzle -> Bool
prop_blank s = let rs        = rows s
                   Pos (x,y) = blank s
               in isNothing ((rs !! x) !! y)
                
prop_listReplaceOp :: [a] -> (Int, a) -> Bool
prop_listReplaceOp s (i,x) = length s == length (s !!= (i, x))

prop_update :: Puzzle -> Pos -> Maybe Int -> Bool
prop_update s p m = let Pos (r,c) = p
                        s' = update s p m
                        rs = rows s'
                    in
                     (rs !! r) !! c == m

-- run with fewerCheck if you
-- do not like to wait...
prop_solve :: Puzzle -> Bool
prop_solve s 
    | solution == Nothing = True
    | otherwise           = isSolutionOf (fromJust solution) s
  where solution = solve s

fewerCheck prop = quickCheckWith (stdArgs{ maxSuccess = 30 })  prop


{-- Template Haskell Magic, ignore this for now! --}
return []
runTests = $quickCheckAll
