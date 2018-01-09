lucky :: (Integral a) => a -> String
lucky 7 = "yooo"
lucky x = "nahhh"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "nope!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "nothing here"
tell (x:[]) = "one element: " ++ show x
tell (x:y:[]) = "two elements: " ++ show x ++ ", " ++ show y
tell (_:_:_) = "too many elements for this old boi"

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (n:ns) = n + sum' ns

randomShit :: Int -> String
randomShit shit
  | shit < (-1) = "too small"
  | abs shit < 1 = "just right"
  | otherwise = "who the hell do you think you are?"
