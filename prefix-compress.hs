-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad 

main = do
	first <- getLine
	second <- getLine
	mapM_ (putStrLn . withLength) $ findPrefix first second

withLength :: String -> String
withLength xs = (show (length xs)) ++ " " ++ xs

findPrefix :: String -> String -> [String]
findPrefix first second = [pref, (drop l first), (drop l second)]
    where pref = prefix first second
          l = length pref

prefix :: String -> String -> String
prefix [] _ = []
prefix _ [] = []
prefix (x:xs) (y:ys) 
    | (x == y) = x : (prefix xs ys)
    | otherwise = []