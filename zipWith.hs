-- Takes a function f(a, b) returns c
-- Zips two lists combining elements through f
-- e.g. zipWith (+) [1, 2, 3] [1, 2, 3] = [2, 4, 6]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
