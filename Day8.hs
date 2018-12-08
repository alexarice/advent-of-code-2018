module Day8 where

import Text.Parsec
import Data.Tree
import Control.Monad

parseTree :: Parsec String st (Tree [Int])
parseTree = do
  children <- getInt
  meta <- getInt
  childTrees <- replicateM children parseTree
  vars <- replicateM meta getInt
  return $ Node vars childTrees where
    getInt = read <$> many1 digit >>= \n -> space >> return n

readParseFile :: IO (Tree [Int])
readParseFile = do
  inputData <- readFile "./day8data.txt"
  case parse parseTree "" inputData of
    Right x -> return x
    Left y  -> error (show y)

part1 :: Tree [Int] -> Int
part1 = sum . concat . flatten

part2 :: Tree [Int] -> Int
part2 = foldTree f where
  f xs [] = sum xs
  f xs ys = g xs ys
  g [] _ = 0
  g (x:xs) ys = getIfExist x ys + g xs ys
  getIfExist _ [] = 0
  getIfExist n (y:ys)
    | n == 0 = 0
    | n == 1 = y
    | otherwise = getIfExist (n-1) ys
