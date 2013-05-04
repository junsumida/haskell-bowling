import Data.Char

result_perfect   = "XXXXXXXXXXXX"
result_nums_only = "9-7-4-5-6-2-3-5-7-9-" 
result_nums_only = "9/7/4/5-6-2-3-5-7-9-" 

score (x:xs)
  | xs == []  = 0 :: Int
  | x == '-'  = score xs 
-- | x == '/'  = 
  | otherwise = digitToInt x + score xs
--score [] = 0 
--score (x:xs) = x : score xs

sample = [1,2,3,4]
sum' (x:[]) = x 
sum' (x:xs) = x + sum' xs
