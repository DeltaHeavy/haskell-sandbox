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


--
-- median-of-three implementation
-- NOTE: not done yet
--
median :: [Int] -> Int
median xs = (quicksort xs) !! (length xs `div` 2)

quickersort :: [Int] -> [Int]
quickersort [] = []
quickersort xss
    | length xss < 3 = quicksort' xss
    | otherwise = (quickersort lesser) ++ [p] ++ (quickersort greater) where lesser = filter (<= p) xss
                                                                             greater = filter (> p) xss
                                                                             p = median three
                                                                             three = ([head xss] ++ [ xss !! midindex ] ++ [(last xss)])
                                                                             midindex = (length xss) `div` 2
