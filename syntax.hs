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


max' :: (Ord a) => a -> a -> a
max' a b
  | a >= b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

randomCrap :: (RealFloat a) => a -> a -> String
randomCrap a b
  | abs crap < 1 = "Conservative"
  | abs crap < 5 = "A bit dangerous"
  | otherwise = "Show off"
  where crap = a/b^2

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l]
  where (f:_) = firstName
        (l:_) = lastName

calcBmis :: RealFloat a => [(a,a)] -> [a]
calcBmis bmis = [bmi weight height | (weight, height) <- bmis]
  where bmi weight height = weight / height ^ 2

calcBmis' :: RealFloat a => [(a,a)] -> [a]
calcBmis' bmis =
  let bmi weight height = weight / height ^ 2
  in [bmi weight height | (weight, height) <- bmis]

calcBmis'' :: RealFloat a => [(a,a)] -> [a]
calcBmis'' bmis = [bmi | (w, h) <- bmis, let bmi = w / h^2]

head'' :: [a] -> a
head'' xs = case xs of [] -> error "Nope!"
                       (x:_) -> x
