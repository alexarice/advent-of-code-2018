module Day5 where

import Data.Char
import Data.List

cancel :: Char -> Char -> Bool
cancel x y = toLower x == toLower y && x /= y

reduce :: String -> String
reduce = reduce' []  where
  reduce' [] [] = []
  reduce' [] (x:xs) = reduce' [x] xs
  reduce' ys [] = reverse ys
  reduce' (y : ys) (x : xs) = if cancel x y then reduce' ys xs else reduce' (x : y : ys) xs

part1 :: String -> Int
part1 = length . reduce . filter isAlpha

getFile :: IO String
getFile = readFile "day5data.txt"


part2 :: String -> Int
part2 xs = fst.minimum.sort $ removers <*> [(reduce . filter isAlpha) xs] where
  removers = map (\x -> append x . length. reduce.filter ((/=x) . toLower)) ['a'..'z']
  append x y = (y,x)
