import Data.List
import Data.Tuple

maximum' :: (Ord a) => [a] -> a
maximum' xs
  | length xs == 1 || head xs >= smallerMax = head xs
  | otherwise = smallerMax
  where smallerMax = maximum' (tail xs)

quickSort :: (Ord a) => [a] -> [a]
quickSort xs
  | xs == [] = []
  | otherwise = quickSort leftSide ++ pivot ++ quickSort rightSide
  where [leftSide, pivot, rightSide] = getPartitions xs



getPartitions :: (Ord a) => [a] -> [[a]]
getPartitions xs = [fst partitions, [maximum xs], snd partitions]
  where partitions = partition leftSide (removePivot xs)
        leftSide x = x < maximum xs

removePivot :: (Ord a) => [a] -> [a]
removePivot xs = delete (maximum xs) xs
