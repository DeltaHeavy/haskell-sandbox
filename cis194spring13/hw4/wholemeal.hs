-- Max Zinkus
-- CIS 194 Spring '13
-- hw4

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl1 (\acc -> \x -> (if (even x) then (x-2) else 1) * acc)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

main :: IO ()
main = do
    putStrLn $ show (fun1 [1,8,8,5,6,10,10,3,4,3])
    putStrLn $ show (fun1' [1,8,8,5,6,10,10,3,4,3])
