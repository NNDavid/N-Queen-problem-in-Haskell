module NQueen where
import Data.List

makeCoordinate::[[Int]] -> [[(Int,Int)]]
makeCoordinate []=[]
makeCoordinate (x:xs)=zip [1..] x:makeCoordinate xs

isValidCoordinate:: (Int,Int) -> [(Int,Int)] -> Bool
isValidCoordinate _ []=True
isValidCoordinate coord (x:xs)
  | fst coord - snd coord ==fst x - snd x || fst coord + snd coord == fst x + snd x = False
  |otherwise = isValidCoordinate coord xs

isValidStand::[(Int,Int)] -> Bool
isValidStand [] = True
isValidStand (x:xs)
  |isValidCoordinate x xs = isValidStand xs
  |otherwise = False

filterList::[[(Int,Int)]] -> [[(Int,Int)]]
filterList [] = []
filterList (x:xs)
  |isValidStand x = x:filterList xs
  |otherwise = filterList xs

queen::Int -> [[(Int,Int)]]
queen n =filterList (makeCoordinate (permutations [1..n]))
  



