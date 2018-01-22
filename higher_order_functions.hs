applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- -- --
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
--
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x
--
flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f y x = f x y

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldl (\acc y -> if x == y then True else acc) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\y acc -> (f(y):acc)) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' xs = foldr1 (\y acc -> if y > acc then y else acc) xs

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\y acc -> if y > acc then y else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc ) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr(\x acc -> if f(x) then (x:acc) else acc) []

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

sqrtSums' :: Int
sqrtSums' = (length $ takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]) + 1
