-- find the largest number under 100,000 that's divisible by 3829

largestDivisible :: Integral a => a -> a
largestDivisible n = head [x | x <- [n,n-1..], x `mod` 3829 == 0]
