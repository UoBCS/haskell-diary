import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
           let (first, gen')   = random gen
               (second, gen'') = random gen'
               (third, _)      = random gen''
           in (first, second, third)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (x, newGen) = random gen in x:randoms' newGen
