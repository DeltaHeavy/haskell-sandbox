-- Max Zinkus
-- hw3 CIS 194 Spring '13
--

skips :: [a] -> [[a]]

skip [] _ = []
skip l n = (head (d l)) : skip (tail l) d

skips s = foldl (:) (zipWith skip s [0..(length s) - 1])

main :: IO ()
main = do
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []
    putStrLn $ (show (skips "ABCD")) ++ " == " ++ (show ["ABCD", "BD", "C", "D"])
    putStrLn $ (show (skips "hello!")) ++ " == " ++ (show ["hello!", "el!", "l!", "l", "o", "!"])
    putStrLn $ (show (skips ([1] :: [Int]))) ++ " == " ++ (show ([[1]] :: [[Int]]))
    putStrLn $ (show (skips [True,False])) ++ " == " ++ (show [[True, False], [False]])
    putStrLn $ (show (skips ([] :: [Int]))) ++ " == " ++ (show ([] :: [Int]))
