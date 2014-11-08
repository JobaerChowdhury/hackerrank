-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

main = do
    line <- getLine
    ls <- getContents
    let inputData = lines ls
    mapM_ (putStrLn . solve) inputData

solve :: String -> String
solve xs = (show (multiples (read xs :: Int)))

multiplesNSum :: Int -> Int -> Int
multiplesNSum num x = num * (n * (n + 1 ) `div` 2)
                     where n = (x-1) `div` num

multiples3Sum :: Int -> Int
multiples3Sum = multiplesNSum 3

multiples5Sum :: Int -> Int
multiples5Sum = multiplesNSum 5

multiples15Sum :: Int -> Int
multiples15Sum = multiplesNSum 15

multiples :: Int -> Int
multiples x = (multiples3Sum x) + (multiples5Sum x) - (multiples15Sum x)