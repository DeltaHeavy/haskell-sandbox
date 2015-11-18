-- Quicksort
-- O(n log n)
--

quicksort :: Ord a => [a] -> [a]        -- type signature (takes a list of orderables, returns a list of orderables)
quicksort [] = []                       -- case empty list, return empty list
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where lesser = filter (<= p) xs
          greater = filter (> p) xs



--
-- listcomp implementation
quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (p:xs) = (quicksort' [y | y <- xs, y <= p])  ++ [p] ++ (quicksort' [y | y <- xs, y > p])
