import Data.List 

solve :: String -> String 
solve ns = show (fst (maximumBy (\x y -> compare (snd x) (snd y)) ( map (\x -> (x, (chainLength x))) [1..(read ns :: Int)])))

chainLength :: Int -> Int 
chainLength n = length (collatz n)

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n 
    | (even n) = n : (collatz (n `div` 2))
    | otherwise = n : (collatz (3 * n + 1))