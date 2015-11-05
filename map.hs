-- this is why I love Haskell
map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []
map' f (x:xs) = f x : map f xs 
