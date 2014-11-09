import Control.Monad 

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]

choseK :: Integer -> Integer -> Integer
choseK n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))

solve :: (Integer, Integer) -> Integer
solve (n,k) = (choseK n k) `mod` (10 ^ 8 + 7)

main = do
	t <- getLine
	cs <- getContents 
	let ws = map words (lines cs)
	let test = map (\[x,y] -> (read x :: Integer, read y :: Integer)) ws
	mapM_ (putStrLn . show . solve) test
