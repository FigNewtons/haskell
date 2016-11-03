module Tuple where

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f p = f (fst p) (snd p)

swap' :: (a, b) -> (b, a)
swap' (x, y) = (y, x)

zip' :: (a, b) -> (c, d) -> ((a, c), (b, d))
zip' t1 t2 = ((fst t1, fst t2), (snd t1, snd t2))

flipzip' :: (a, b) -> (c, d) -> ((b, d), (a, c))
flipzip' t1 t2 = swap' $ zip' t1 t2

pairMap' :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
pairMap' f g (x, y) = (f x, g y)

diagonal' :: a -> (a, a)
diagonal' x = (x, x)
