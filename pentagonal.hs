incSec :: [Integer]
incSec = [4,7..]

pentagon :: [Integer]
pentagon = 0:1:zipWith (+) incSec (tail pentagon)

solveInt :: Int -> Integer
solveInt n = pentagon !! n

solve :: String -> String
solve ls = show (solveInt (read ls :: Int))

main = do
    n <- getLine
    cs <- getContents    
    mapM_ (putStrLn . solve) (lines cs)