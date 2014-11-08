-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad 

main = do
    t <- getLine
    content <- getContents 
    let ls = lines content
    mapM_ (putStrLn . solve) ls

solve :: String -> String
solve ns = (show result) where result = (seriesSquareOfSum (read ns))- (seriesSumOfSquares (read ns))

seriesSumOfSquares :: Int -> Int
seriesSumOfSquares n = (n * (n+1) * (2 * n + 1)) `div` 6

seriesSquareOfSum :: Int -> Int
seriesSquareOfSum n = sum * sum where sum = (n * (n +1)) `div` 2