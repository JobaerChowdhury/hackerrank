-- Enter your code here. Read input from STDIN. Print output to STDOUT
main = do
	line <- getLine
	let ns = head (words line)
	let ks = head (tail (words line))
	putStrLn $ show (superDigit ns (read ks :: Integer))

firstSum :: [Char] -> Integer
firstSum ns = sum (map (\x -> (read [x] :: Integer)) ns)

superDigit :: String -> Integer -> Integer
superDigit m k = superDigit' n 
                   where n = k * (firstSum m)

superDigit' :: Integer -> Integer
superDigit' x = if ds < 10 then ds 
	                      else superDigit' ds
	                      where ds = digsum x

superDigitk :: Integer -> Integer
superDigitk n = (n - 1) `mod` 9 + 1

digsum x | x < 10 = x
         | otherwise = (x `mod` 10) + (digsum (x `div` 10)) 	                   