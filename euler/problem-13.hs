-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad
import Data.List

main = do 
  ts <- getLine
  contents <- getContents 
  putStrLn $ solve (lines contents)
  
solve :: [String] -> String
solve ns = intercalate "" (map show (take 10 (digits (sum(map (\x -> read x :: Integer) ns)))))

digits = reverse . digitsRev

digitsRev :: Integer -> [Integer]
digitsRev n 
    | n < 10 = [n]
    | otherwise = n `mod` 10 : (digitsRev (n `div` 10))
