-- Input: a list P of points in the plane.

-- Sort the points of P by x-coordinate (in case of a tie, sort by y-coordinate).

-- Initialize U and L as empty lists.
-- The lists will hold the vertices of upper and lower hulls respectively.

-- for i = 1, 2, ..., n:
--     while L contains at least two points and the sequence of last two points
--            of L and the point P[i] does not make a counter-clockwise turn:
--        remove the last point from L
--    append P[i] to L

-- for i = n, n-1, ..., 1:
--    while U contains at least two points and the sequence of last two points
--            of U and the point P[i] does not make a counter-clockwise turn:
--        remove the last point from U
--    append P[i] to U

-- Remove the last point of each list (it's the same as the first point of the other list).
-- Concatenate L and U to obtain the convex hull of P.
-- Points in the result will be listed in counter-clockwise order.

import Text.Printf
import Data.List

type Point = (Int, Int)

crossProduct :: Point -> Point -> Point -> Int
crossProduct (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

dedupsort :: (Ord a) => [a] -> [a]
dedupsort = map head . group . sort 

-- points should be sorted in lexicographic order
lowerHull :: [Point] -> [Point]
lowerHull ps = foldl hullHelper [] ps

-- Helper function to compute hulls. The list will contain the current
-- points in the hull. 
hullHelper :: [Point] -> Point -> [Point]
hullHelper hs p = counterPoints ++ [p]
    where counterPoints = computeCounterPoints hs p

-- while p and the last two points of hs doesn't make a counter clock turn, pop hs
computeCounterPoints :: [Point] -> Point -> [Point]
computeCounterPoints [] _ = []
computeCounterPoints hs@(x:[]) _ = hs 
computeCounterPoints hs p =  if cr <= 0 then computeCounterPoints (init hs) p else hs
    where ll = last hs
          sl = last (init hs)
          cr = crossProduct sl ll p  


upperHull :: [Point] -> [Point]
upperHull ps = foldl hullHelper [] (reverse ps)

convexHull :: [Point] -> [Point]
convexHull ps = (init lower) ++ (init upper)
    where sortedPoints = dedupsort ps
          lower = lowerHull sortedPoints
          upper = upperHull sortedPoints

distance :: Point -> Point -> Double
distance (ax, ay) (bx, by) = sqrt ( (x2 - x1) * (x2 - x1) + (y2-y1) * (y2 - y1))
    where x1 = fromIntegral ax :: Double
          y1 = fromIntegral ay :: Double
          x2 = fromIntegral bx :: Double
          y2 = fromIntegral by :: Double

totalDist :: [Point] -> Double
totalDist ls = sum (map (\(a, b) -> distance a b) ((head ls, last ls) : (zip (init ls) (tail ls))))

solve :: [Point] -> Double
solve points = totalDist $ convexHull points

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans