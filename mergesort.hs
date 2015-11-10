-- Another O(N log N) sort (mergesort)
-- a little more machinery gives us O(N log N) in all cases
-- and it generally more efficient in Haskell's internal implementation

-- split takes a list and returns two lists
-- it pulls two elements off the front and prepends them to the two return lists
-- before recursing
split :: (Ord a) => [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys)

-- merge takes two lists and returns one
-- this one's easy, use guards to recurse and merge prepending the lower element
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- mergesort is the driver, taking a list of orderables and returning the sorted list
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort xsl) (mergesort xsr)
    where (xsl, xsr) = split xs
