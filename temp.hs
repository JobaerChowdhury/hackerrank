module Rot13 where

import Data.Char

isChar :: Char -> Bool
isChar x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')

transform :: Char -> Char
transform x 
  | isChar x = chr ((ord x) + 13)
  | otherwise = x

rot13 :: String -> String
rot13 = map transform