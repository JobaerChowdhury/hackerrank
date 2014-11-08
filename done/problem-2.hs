import Control.Monad

cycles l 0 = l
cycles l n 
  | n `mod` 2 == 1 = 2 * (cycles l (n-1))
  | n `mod` 2 == 0 = 1 + (cycles l (n-1))

main = do
    val1 <- readLn
    inputs <- replicateM val1 readLn
    let results = map (cycles 1) inputs        
    mapM_ (print) results