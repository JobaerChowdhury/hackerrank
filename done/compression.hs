import Data.List 

compress :: String -> String
compress xs = concat (map withLength (group xs))


withLength :: String -> String
withLength [] = []
withLength (x:[]) = (x:[])
withLength (x:xs) = [x] ++ (show (length (x:xs)))

