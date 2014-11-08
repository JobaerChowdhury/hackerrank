-- Enter your code here. Read input from STDIN. Print output to STDOUT
main = do
    ns <- getLine
    putStrLn (show (solve (read ns)))

solve :: Integer -> Integer
solve n = sum (filter myFilter [10..(n-1)])

myFilter :: Integer -> Bool
myFilter n = ds `mod` n == 0 
    where ds = (digitsFactSum n)

digitsFactSum :: Integer -> Integer
digitsFactSum n = sum $ map factorial (digits n)

digits = reverse . digitsRev

digitsRev :: Integer -> [Integer]
digitsRev n 
    | n < 10 = [n]
    | otherwise = n `mod` 10 : (digitsRev (n `div` 10))

factorial :: Integer -> Integer
factorial n
	| n == 0 = 1
	| otherwise = product [1..n]


