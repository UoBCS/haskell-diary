-- Gaussian elimination (no pivoting implementation)

gaussianElim :: (Num a, Ord a) => [[a]] -> [a] -> [[a]]
gaussianElim [] = []
