import Data.List 

main = do
	ns <- getLine
	let n = (read ns) :: Int
	let res = reverse (pascal n)
	mapM_ putStrLn $ concat (map transform res)

transform :: [Int] -> [String]
transform ns = [intercalate " " (map (\x -> (show x)) ns)]

pascal :: Int -> [[Int]]
pascal n
    | n <= 1  = [[1]]
    | otherwise = buildRow (head prev) : prev
                    where prev = pascal (n - 1)


buildRow :: [Int] -> [Int]
buildRow xs = map (\(a,b) -> a + b) $ zip ps (tail ps)
                 where ps = (0 : xs) ++ [0]