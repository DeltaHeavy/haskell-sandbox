-- guard implementation
sumWithPredicate :: (Int -> Bool) -> [Int] -> Int
sumWithPredicate pred list
    | list == [] = 0
    | pred x = x + sumWithPredicate pred xs
    | otherwise = 0 + sumWithPredicate pred xs
    where x:xs = list

-- listcomp implementation
sumWithPredicate' :: (Int -> Bool) -> [Int] -> Int
sumWithPredicate' pred list = sum [x | x <- list, pred x]
