module Base where

-- Euclidean algorithm
gcd :: (Integral a) => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
          where gcd' a 0 = a
                gcd' a b = gcd' b (a `rem` b)

lcm :: (Integral a) => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm a b = abs (y * quot x (gcd x y))

coprimes :: (Integral a) => a -> [(a,a)]
coprimes n = [ (i, j) | i <- [1..n], j <- [1..i-1], gcd i j == 1]

