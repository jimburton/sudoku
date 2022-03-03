module Main 
    where
import Sudoku

main :: IO ()
main = do s <- readAndSolve "../puzzles/easy10.sud"
          case s of
            Nothing  -> putStrLn "Solving puzzle failed"
            (Just s) -> printPuzzle s
