import Data.List
import Control.Monad

main = do
	n <- getLine
	inputdata <- getContents 
	let ls = lines inputdata
	let res = process ls
	let fs = map (\x -> (filterElements (snd x) (fst x))) res
	mapM_ putStrLn $  concat (map transform fs)

transform :: [Integer] -> [String]
transform [] = ["-1"]
transform ns = [intercalate " " (map (\x -> (show x)) ns)]

process :: [String] -> [(Int, [Integer])]
process [] = []    
process (x : y : rest) = (single x y) : (process rest)

single :: String -> String -> (Int, [Integer])
single ks ns = (k, nums) 
                   where k = read (head (tail (words ks))) :: Int
                         nums = map (\x -> read x :: Integer) (words ns)

filterElements :: [Integer] -> Int -> [Integer]
filterElements as k = nub (filter (\x -> x `elem` dup) as)
                         where 
                         	sorted = sort as
                         	grouped = group sorted
                         	largers = filter (\x -> (length x) >= k) grouped
                         	dup = map (\x -> head x) largers
