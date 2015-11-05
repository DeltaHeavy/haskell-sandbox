-- O(n log n)
-- where-clause-filter implementation
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) -- lazy evaluation O(n)
    where lesser = filter (<= p) xs -- O(n / 2) multiplicative
          greater = filter (> p) xs -- O(n / 2) multiplicative

-- listcomp implementation
quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (p:xs) = (quicksort' [y | y <- xs, y <= p])  ++ [p] ++ (quicksort' [y | y <- xs, y > p])
