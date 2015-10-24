fizzBuzz :: Int -> [[Char]]
fizzBuzz n = [ if x `mod` 15 == 0 then "FizzBuzz" else if x `mod` 3 == 0 then "Fizz" else if x `mod` 5 == 0 then "Buzz" else show x | x <- [1..n] ]
