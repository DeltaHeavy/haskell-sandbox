-- mergesort
-- a little more machinery gives us O(N log N) in all cases

-- split takes a list and returns a tuple of lists
-- it pulls two elements off the front and prepends them to the two return lists
-- before recursing
split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys)

-- merge takes two lists and returns one
-- this one's easy, use guards to prepend the lower element and then
-- recurse with the remainder of the list from which we prepended
-- and the whole list which we didn't touch
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xxs@(x:xs) yys@(y:ys)
    | x <= y = x : merge xs yys
    | otherwise = y : merge xxs ys

-- mergesort takes a list of orderables and returns the sorted list
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (xsl, xsr) = split xs in merge (mergesort xsl) (mergesort xsr)
