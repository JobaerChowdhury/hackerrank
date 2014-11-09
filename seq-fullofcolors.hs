countItems :: Eq a => [a] -> a -> Int
countItems ls e = length $ filter (\x -> x == e) ls

inverse :: Char -> Char
inverse 'R' = 'G'
inverse 'G' = 'R'
inverse 'Y' = 'B'
inverse 'B' = 'Y'


folder :: (String, Bool) -> Char -> (String, Bool)
folder ([], _) c = ([c], False)
folder (a:[], _) =  