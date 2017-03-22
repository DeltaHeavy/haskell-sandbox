-- Max Zinkus
-- cis194spring13
-- hw3
-- skips

import qualified Data.List (sort)

miss :: [a] -> Int -> [a]
miss [] _ = []
miss l 0 = l
miss l n = miss (tail l) (n-1)

skip :: [a] -> Int -> [a]
skip [] _ = []
skip l n = (take 1 foo) ++ (skip (drop 1 foo) n)
            where foo = (miss l n)

skips :: [a] -> [[a]]
skips l = map (skip l) [0..(length l)-1]

-- local maxima
locale :: [a] -> Int -> [a]
locale l n = [l !! x | x <- [(n-1)..(n+1)]]

foldM :: Maybe a -> [a] -> [a]
foldM x acc = case x of
                (Just c) -> c : acc
                Nothing -> acc

local :: [Int] -> Maybe Int
local [x,y,z]
    | x < y && y > z = Just y
    | otherwise = Nothing
local _ = Nothing

maxima :: [Int] -> [Int]
maxima l = foldr foldM [] (map (local . (locale l)) [1..((length l) - 2)])

-- histogram

count :: Integer -> [Integer] -> (Integer, Int)
count n [] = (n, 0)
count n xxs@(x:xs) = if x == n then (n, (length (takeWhile (\z -> z == n) xxs))) else count n xs

right :: (a, b) -> b
right (_, y) = y

printhist :: Int -> [(Integer,Int)] -> String
printhist _ [] = "\n"
printhist n (x:xs) = (if (right x) >= n then '*' else ' ') : (printhist n xs)

histo :: Int -> [(Integer,Int)] -> String
histo 0 _ = "==========\n0123456789"
histo n l = (printhist n l) ++ (histo (n-1) l)

histogram :: [Integer] -> String
histogram l = histo maxcnt counts 
                where s = Data.List.sort l
                      counts = map (\c -> count c s) [0..9]
                      maxcnt = foldl max 0 (map right counts) 

main :: IO ()
main = do 
    putStrLn "skips"
    putStrLn $ show (skips "ABCD") -- ["ABCD", "BD", "C", "D"]
    putStrLn $ show (skips "hello!") -- ["hello!", "el!", "l!", "l", "o", "!"]
    putStrLn $ show (skips ([1] :: [Int])) -- [[1]]
    putStrLn $ show (skips [True, False]) -- [[True, False], [False]]
    putStrLn $ show (skips ([] :: [Int])) -- []
    putStrLn "\nlocal maxima"
    putStrLn $ show (maxima ([1,2,1,4,5,2] :: [Int])) -- [2,5]
    putStrLn $ show (maxima ([1,2,4,1,2,1,3,2,3] :: [Int])) -- [4, 2, 3]
    putStrLn $ show (maxima ([2,9,5,6,1] :: [Int])) -- [9, 6]
    putStrLn $ show (maxima ([2,3,4,1,5] :: [Int])) -- [4]
    putStrLn $ show (maxima ([1,2,3,4,5] :: [Int])) -- []
    putStrLn "\nhistogram\n"
    putStrLn $ histogram [1,3,1,3,5,9,9,0,2,5,5,5]
    putStrLn ""
    putStrLn $ histogram [3,5]
