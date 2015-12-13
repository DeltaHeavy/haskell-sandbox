fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n*fact(n-1)

fact' :: (Integral a) => a -> a
fact' n = foldl1 (*) [2..n]

main = do
    putStrLn $ show (fact 10)
    putStrLn $ show (fact' 10)
    putStrLn $ show (fact 20)
    putStrLn $ show (fact' 20)
