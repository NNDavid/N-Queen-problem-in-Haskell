module NQueen where
import Data.List

isValidCoordinate:: (Int,Int) -> [(Int,Int)] -> Bool
isValidCoordinate _ []=True
isValidCoordinate coord (x:xs)
  |fst coord - snd coord ==fst x - snd x || fst coord + snd coord == fst x + snd x = False
  |otherwise = isValidCoordinate coord xs

isValidStand::[(Int,Int)] -> Bool
isValidStand [] = True
isValidStand (x:xs)
  |isValidCoordinate x xs = isValidStand xs
  |otherwise = False

solveProblem::[[Int]] -> [[(Int,Int)]]
solveProblem []=[]
solveProblem (x:xs)
  |isValidStand (zip [1..] x) = zip[1..] x:solveProblem xs
  |otherwise = solveProblem xs

queen::Int -> [[(Int,Int)]]
queen n =solveProblem (permutations [1..n])
  



