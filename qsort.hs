main = putStr =<< return . unlines . qsort . lines =<< getContents

qsort :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (p:xs) = qsort lt ++ [p] ++ qsort gteq
                 where
                   lt   = [x | x <- xs, x < p]
                   gteq = [x | x <- xs, x >= p]
