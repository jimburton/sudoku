module Main where

import Data.Maybe ( isNothing
                  , fromJust )
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Sudoku

-------------------------------------------------------------------------
-- QuickCheck tests:
--
-- Run these using cabal, as
--
-- sudoku$ cabal run test-sudoku
--
--
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
                 && all ((==9) . length) rs
                 && and (concatMap (map isNothing) rs)

prop_isPuzzle :: Puzzle -> Bool
prop_isPuzzle s = isPuzzle s

prop_isNotPuzzle :: Bool
prop_isNotPuzzle = not $ isPuzzle (Puzzle [[]])

prop_blocks :: Puzzle -> Bool
prop_blocks s = (length bl == 3*9) && 
                and [length b == 9 | b <- bl]
  where bl = blocks s

prop_isValidPuzzle :: Puzzle -> Bool
prop_isValidPuzzle s = isValidPuzzle s || not (null bads)
  where bads = filter (not . isValidBlock) (blocks s)

prop_blank :: Puzzle -> Bool
prop_blank s = let rs        = rows s
                   Pos (x,y) = blank s
               in isNothing ((rs !! x) !! y)
                
prop_listReplaceOp :: [Int] -> (Int, Int) -> Bool
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
    | isNothing solution  = True
    | otherwise           = isSolutionOf (fromJust solution) s
  where solution = solve s

fewerCheck prop = quickCheckWith (stdArgs{ maxSuccess = 30 })  prop

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "Check that allBlankPuzzle produces a blank puzzle"                 prop_allBlank
        , testProperty "Checks if puzzles are really valid representation of sudoku."      prop_isPuzzle
        , testProperty "Check that invalid puzzles are rejected."                          prop_isNotPuzzle
        , testProperty "Check that the blocks function selects the blocks."                prop_blocks
        , testProperty "Check that all blocks in a puzzle are valid."                      prop_isValidPuzzle
        , testProperty "Check that the blank function can find blank cells."               prop_blank
        , testProperty "Check that the replace operator really replaces values in a cell." prop_listReplaceOp
        , testProperty "Check that puzzles can be updated. "                               prop_update
        , testProperty "Check that puzzles can be solved."                                 prop_solve
        ]

