
skips :: [a] -> [[a]]
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []

skip _ [] = []
skip d l = head l : skip d (d (tail l))

skips s = [skip (drop n) (drop n s) | n <- [0..(length s)-1]]
