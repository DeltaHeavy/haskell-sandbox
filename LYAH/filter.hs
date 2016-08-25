-- You've got to be kidding it's this easy
filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs
