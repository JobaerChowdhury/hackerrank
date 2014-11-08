-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Data.List 
import Control.Monad

main = do
  n <- getLine
  inputData <- getContents
  let ls = lines inputData
  mapM_ (putStrLn . rotateString) ls

rotateString :: String -> String
rotateString ss = intercalate " " (map (\x -> rotate x ss) [1..n])
                          where n = length ss

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs