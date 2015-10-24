bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Whale"

bmiCalc :: (RealFloat a) => a -> a -> String
bmiCalc w h
    | w / h ^ 2 <= 18.5 = "Underweight"
    | w / h ^ 2 <= 25.0 = "Normal"
    | w / h ^ 2 <= 30.0 = "Overweight"
    | otherwise         = "Whale"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' w h
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Whale"
    where bmi = w / h ^ 2

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- the following two functions are equivalent
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  

--
--head' :: [a] -> a  
--head' xs = case xs of [] -> error "No head for empty lists!"  
--                      (x:_) -> x  
--

