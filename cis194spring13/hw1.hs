-- Homework 1
--
-- Credit Card stuff :)

-- Exercise 1

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
        reverser acc (x:xs) = reverser (x:acc) xs

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

-- We're going to implement this in the Rev edition
-- because it's easier to play with linked lists :)
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = ((onesPlace n) : toDigitsRev (div n 10))
        where onesPlace = (`mod` 10)

-- And the non-reversed one will just be the reverse of the toDigitsRev
toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

-- Exercise 2
-- could also be done with a foldr
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
    | (odd (length xs)) = (2*x) : (doubleEveryOther xs)
    | otherwise = x : (doubleEveryOther xs)

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + (sumDigits xs)

-- Exercise 4
validate :: Integer -> Bool
validate x = (mod (sumDigits (doubleEveryOther (toDigits x))) 10) == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src targ aux
    | n <= 0 = []
    | otherwise = (hanoi (n-1) src aux targ) ++ [(src, targ)] ++ (hanoi (n-1) aux targ src)

-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n src targ aux1 aux2
    | n <= 0 = []
    | n < 3 = hanoi n src targ aux1
    | otherwise = 

main :: IO () 
main = do
    putStrLn $ show (toDigitsRev 1234) -- 4,3,2,1
    putStrLn $ show (doubleEveryOther [1,2]) -- 2,2
    putStrLn $ show (doubleEveryOther [1,2,3]) -- 1,4,3
    putStrLn $ show (doubleEveryOther [1,2,3,4]) -- 2,2,6,4 
    putStrLn $ show (sumDigits [16,7,12,5]) -- 22
    putStrLn $ show (validate 4012888888881881) -- true
    putStrLn $ show (validate 4012888888881882) -- false
    -- it's weird, they want us to call "b" the target, whatever
    putStrLn $ show (hanoi 1 "a" "b" "c")
    putStrLn $ show (hanoi 2 "a" "b" "c")
    putStrLn $ show (hanoi 3 "a" "b" "c")
    putStrLn $ show (length (hanoi 15 "a" "b" "c")) -- 32767
