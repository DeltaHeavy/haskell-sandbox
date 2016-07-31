sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- note that this could use foldl and ++, but ++ is O(n) and : is O(1)

fact' :: (Integral a) => a -> a
fact' n = foldl (*) 1 [2..n]
-- fold left starting with 1

fact'' :: (Enum a, Num a, Ord a) => a -> a
fact'' n
    | n <= 1 = 1
    | otherwise = foldr1 (*) [2..n]
-- fold right, accumulate with 1

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\acc x -> if x == y then True else acc) False
-- omit ys from parameters and end of right side, return a function instead
