main = do
	n <- getLine
	content <- getContents
	let ls = lines content
	mapM_ (putStrLn . swapString) ls

swapString :: String -> String
swapString [] = []
swapString (x:[]) = [x]
swapString (x:y:xs) = y : x : swapString xs