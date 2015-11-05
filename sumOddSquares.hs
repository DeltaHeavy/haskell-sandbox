--  find the sum of all odd squares that are smaller than 10,000
sumOddSquares :: (Integral a) => a -> a
sumOddSquares n = sum (takeWhile (< n) (filter odd (map (^2) [1..])))
