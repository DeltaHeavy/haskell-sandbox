-- O(n log n)
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) -- lazy evaluation O(n)
    where lesser = filter (<= p) xs -- O(n / 2) multiplicative
          greater = filter (> p) xs -- O(n / 2) multiplicative
