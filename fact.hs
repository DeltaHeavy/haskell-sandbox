fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n*fact(n-1)

fact' :: (Integral a) => a -> a
fact' n = foldl (*) 1 [2..n]
