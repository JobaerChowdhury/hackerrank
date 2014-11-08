-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

main = do
    ts <- getLine
    content <- getContents 
    mapM_ (putStrLn . solve) (lines content)

solve :: String -> String
solve ns = show $ sum (digitsRev (2 ^ (read ns :: Integer)))

digits = reverse . digitsRev

digitsRev :: Integer -> [Integer]
digitsRev n 
    | n < 10 = [n]
    | otherwise = n `mod` 10 : (digitsRev (n `div` 10))
