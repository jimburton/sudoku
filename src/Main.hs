module Main (main) 
    where
import Sudoku ( printPuzzle, readAndSolve )

main :: IO ()
main = do s <- readAndSolve "../puzzles/easy10.sud"
          case s of
            Nothing  -> putStrLn "Solving puzzle failed"
            (Just s) -> printPuzzle s
