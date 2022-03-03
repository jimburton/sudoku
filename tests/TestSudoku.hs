module Main where

import Data.Maybe ( isNothing
                  , fromJust )
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Sudoku

-- * QuickCheck tests:

-- | Run these using cabal, as
-- |
-- | $ cabal run test-sudoku
-- |

-- | Generates an arbitrary cell in a Puzzle.
cell :: Gen (Maybe Int)
cell = frequency [ (9, return Nothing)
                 , (1, do n <- choose(1,9) ; return (Just n))]

-- | An instance for generating arbitrary Puzzles.
instance Arbitrary Puzzle where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Puzzle rows)

-- | An instance for generating arbitrary Pos values.
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
prop_isPuzzle = isPuzzle

prop_isNotPuzzle :: Bool
prop_isNotPuzzle = not $ isPuzzle (Puzzle [[]])

prop_blocks :: Puzzle -> Bool
prop_blocks p = (length bl == 3*9) && 
                and [length b == 9 | b <- bl]
  where bl = blocks p

prop_isValidPuzzle :: Puzzle -> Bool
prop_isValidPuzzle p = isValidPuzzle p || not (null bads)
  where bads = filter (not . isValidBlock) (blocks p)

prop_blank :: Puzzle -> Bool
prop_blank p = let rs        = rows p
                   Pos (x,y) = blank p
               in isNothing ((rs !! x) !! y)
                
prop_listReplaceOp :: [Int] -> (Int, Int) -> Bool
prop_listReplaceOp p (i,x) = length p == length (p !!= (i, x))

prop_update :: Puzzle -> Pos -> Maybe Int -> Bool
prop_update p pos m = let Pos (r,c) = pos
                          s'        = update p pos m
                          rs        = rows s'
                    in
                     (rs !! r) !! c == m

-- run with fewerCheck if you
-- do not like to wait...
prop_solve :: Puzzle -> Bool
prop_solve p 
    | isNothing solution  = True
    | otherwise           = isSolutionOf (fromJust solution) p
  where solution = solve p

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
        , testProperty "Check that puzzles can be updated."                                prop_update
        , testProperty "Check that puzzles can be solved."                                 prop_solve
        ]

