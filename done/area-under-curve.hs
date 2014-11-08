import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r as bs = [sum (map (\x -> (f x) * 0.001) cis), pi * (sum (map (\x -> (f x) * (f x) * 0.001) cis))]
    where f = createFunction as bs
          cis = intervals l r


intervals :: Int -> Int -> [Double]
intervals l r = map (\x -> ( (fromIntegral l)+ (fromIntegral x) * 0.001)) [1..n]
                  where n = (r - l) * 1000

createFunction :: [Int] -> [Int] -> (Double -> Double)
createFunction as bs = \x -> sum (map (\(a,b) -> (fromIntegral a) *  x ^ b) (zip as bs))

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
