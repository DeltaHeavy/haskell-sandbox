doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
    then x
    else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
length' xs = sum [1 | _ <- xs]
lengthr' :: (Num b) => [a] -> b
lengthr' [] = 0
lengthr' (_:xs) = 1 + lengthr' xs
addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

