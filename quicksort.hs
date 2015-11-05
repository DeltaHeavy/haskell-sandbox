-- Quicksort
-- O(n log n)
--
--
quicksort :: Ord a => [a] -> [a] -- type signature (takes a list of orderables, returns a list of orderables)
quicksort [] = [] -- case empty list, return empty list
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) -- lazy evaluation O(n)
    where lesser = filter (<= p) xs -- O(n / 2) multiplicative -> log2 n -> O(log n)
          greater = filter (> p) xs -- O(n / 2) multiplicative -> log2 n -> O(log n)
--
-- n * (log n + log n) -> 2(n log n) -> O(n log n)

-- listcomp implementation
quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (p:xs) = (quicksort' [y | y <- xs, y <= p])  ++ [p] ++ (quicksort' [y | y <- xs, y > p])
