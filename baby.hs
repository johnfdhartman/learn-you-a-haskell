doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' x =
  if x > 100
    then x
    else x * 2
  + 1

boomBang xs = [if x < 10 then "boom" else "bang" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

rightTriangles = [ (a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2 && a > b ]
