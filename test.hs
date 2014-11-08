primes = sieve [2..]

sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]


fibs :: [Integer]
fibs = 1:2:zipWith (+) fibs (tail fibs)