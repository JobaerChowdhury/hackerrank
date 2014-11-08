import Control.Monad

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

solveInt :: Int -> Integer
solveInt n = (fibs !! n) `mod` (10 ^ 8 + 7)

solve :: String -> String
solve ls = show (solveInt (read ls :: Int))

main = do
    n <- getLine
    cs <- getContents
    let ls = lines cs
    mapM_ (putStrLn . solve) ls