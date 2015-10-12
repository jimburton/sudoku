module Main 
    where
import Sudoku
import Data.Maybe (fromJust)

main :: IO ()
main = do s <- readAndSolve "../puzzles/easy10.sud"
          printPuzzle (fromJust s)
