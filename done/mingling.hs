mingle :: String -> String -> String
mingle [] [] = []
mingle (x:[]) (y:[]) = (x:y:[])
mingle (x:xs) (y:ys) = (x:y:[]) ++ (mingle xs ys)
