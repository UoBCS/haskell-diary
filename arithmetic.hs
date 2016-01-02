
-- arithmetic.hs
-- Contains arithmetic functions from 99 challenge

isPrime :: Integral a => a -> Bool
isPrime n = null([a | a <- [2..(n-1)], n `mod` a == 0])

gcd' :: Integral a => a -> a -> a
gcd' a b | b == 0    = a
         | otherwise = gcd b (a `mod` b)

coprime :: Integral a => a -> a -> Bool
coprime a b = gcd' a b == 1

totient :: Integral a => a -> Int
totient 1 = 1
totient m = length $ [r | r <- [1..m], coprime r m]
