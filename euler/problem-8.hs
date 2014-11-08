consecProd :: Int -> [Int] -> [Int]
consecProd n [] = []
consecProd n ds = (product (take n ds)) : (consecProd n (tail ds))

consecProdPadded :: Int -> [Int] -> [Int]
consecProdPadded n ds = consecProd n (ds ++ (replicate n 0))





