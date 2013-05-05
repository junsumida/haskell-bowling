-- for digitToInt
import Data.Char

score all@(x:y:xs)
  | length all < 3 = numOrGtr x y '0'
score all@(x:y:z:xs) 
  | xs == [] && x == 'X' = strike x y z
  | x == 'X' = strike x y z + score (y:z:xs) 
  | x /= 'X' = numOrGtr x y z + score (z:xs)

chToNum x
  | x == '-'  = 0 :: Int
  | x == 'X'  = 10 :: Int
  | otherwise = digitToInt x

numOrGtr x y z
  | y == '/'  = 10 + chToNum z
  | otherwise = chToNum x + chToNum y

strike x y z
  | z == '/'  = 20 :: Int 
  | otherwise = 10 + chToNum y + chToNum z
