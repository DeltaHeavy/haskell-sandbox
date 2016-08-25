-- Homework 1
--
-- Credit Card stuff :)

-- This reverseList is monstrously slow due to traversing linked lists
reverseList''' :: [a] -> [a]
reverseList''' [] = []
reverseList''' (x:xs) = reverseList''' xs ++ [x]

-- this reverseList uses an accumulator in an internal function
-- which prepends each element in the remaining list to the accumulator
reverseList'' :: [a] -> [a]
reverseList'' = reverser []
    where
        reverser acc [] = acc
        reverser acc (x:xs) = (x:acc) xs

-- but working through a list doing something
-- is what fold is for, so we can do this with foldl.
-- (\acc x -> x:acc) is a lambda which takes an accumulator
-- and an element, and prepends it. Thus we work our way
-- left to right prepending the list given as an argument
-- The [] empty list is the initial accumulator, an argument to foldl
reverseList' :: [a] -> [a]
reverseList' = foldl (\acc x -> x:acc) []

-- One last revision. Our lambda (\acc x -> x:acc)
-- takes two arguments and applies : to them, reversed
-- flip takes a function and two arguments to that function,
-- and applies the function on the arguments reversed,
-- for example: mod 5 10 is equal to 5, but flip mod 5 10
-- is equal to mod 10 5, which is equal to 0
-- Thus we can replace our lambda with (flip (:))
reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

toDigits :: Integer -> [Int]
toDigitsRev n
    | n <= 0 = []
    | otherwise = ((onesPlace n) : toDigits (div n 10))
        where onesPlace = (`mod` 10)

toDigits = (reverseList (toDigitsRev))
