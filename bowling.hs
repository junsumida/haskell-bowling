import Data.Char

result_perfect   = "XXXXXXXXXXXX"
result_nums_only = "9-7-4-5-6-2-3-5-7-9-" 
result_nums_spare = "9/7/4/5-6-2-3-5-7-9-" 

--score (x:y:z:xs)
--  | y:z:xs == []  = 0 :: Int
--  | x == '-'  = score y:z:xs
--  | otherwise = digitToInt x + score xs
--score [] = 0 
--score (x:xs) = x : score xs

resToNum x
  | x == '-'  = 0 :: Int
  | x == 'X'  = 10 :: Int
  | otherwise = digitToInt x

numOrGtr x y z
  | y == '/'  = 10 + resToNum z
  | otherwise = resToNum x + resToNum y

strike x y z
  | z == '/'  = 20 :: Int 
  | otherwise = 10 + resToNum y + resToNum z

sample = [1,2,3,4]
sum' (x:[]) = x 
sum' (x:xs) = x + sum' xs
