module Day06 where

import Text.Parsec
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

number :: Parsec String st Int
number = read <$> many1 digit

coordinate :: Parsec String st (Int,Int)
coordinate = do
  x <- number
  _ <- string ", "
  y <- number
  return (x,y)

parseFile :: Parsec String st [(Int,Int)]
parseFile = endBy coordinate endOfLine

getFile :: IO [(Int,Int)]
getFile = do
  inputData <- readFile "./day6data.txt"
  case parse parseFile "" inputData of
    Right x -> return x
    Left y  -> error (show y)

getBounds :: [(Int,Int)] -> (Int,Int,Int,Int)
getBounds xs = let ys = map fst xs
                   zs = map snd xs
               in (minimum ys,maximum ys,minimum zs, maximum zs)

distance :: (Int,Int) -> (Int,Int) -> Int
distance (x,y) (a,b) = abs (x-a) + abs (y-b)

getGrid :: (Int,Int,Int,Int) -> [(Int,Int)]
getGrid (a,b,c,d) = [(x,y) | x <- [a+1..b-1], y <- [c+1..d-1]]

getBoundary :: (Int,Int,Int,Int) -> [(Int,Int)]
getBoundary (a,b,c,d) = [(x,y) | x <- [a..b], y <- [c,d]] ++
                          [(x,y) | x <- [a,b] , y <- [c+1..d-1]]

getClosest :: (Int,Int) -> [(Int,Int)] -> Maybe (Int,Int)
getClosest i = fmap snd.getAnswer . sort . map (\x -> (distance i x, x)) where
  getAnswer (x : y : _) = if fst x == fst y then Nothing else Just x
  getAnswer _ = Nothing

gatherDuplicates :: (Eq a) => [a] -> [a]
gatherDuplicates (x:y:xs) =let ys = gatherDuplicates (y:xs)
                           in if x == y then ys else x:ys
gatherDuplicates xs = xs

part1 :: [(Int,Int)] -> Int
part1 xs = let bounds = getBounds xs
               boundary = getBoundary bounds
               grid = getGrid bounds
               boundaryPoints = gatherDuplicates $ sort $ mapMaybe (`getClosest` xs) boundary
           in  snd.head .filter (\y -> fst y `notElem` boundaryPoints) $ sortBy (\x y -> compare (snd y) (snd x)) $ M.assocs $ foldr insertClosest (M.fromList [(x,0) | x <- xs]) grid where
  insertClosest x table = case getClosest x xs of
    Just y -> M.adjust (+1) y table
    Nothing -> table

getExpandedGrid :: (Int,Int,Int,Int) -> Int -> [(Int,Int)]
getExpandedGrid (a,b,c,d) n = [(x,y) | x <- [a-e .. b+e], y <- [c-e .. d+e]] where
  e = 10000 `div` n

part2 :: [(Int,Int)] -> Int
part2 xs = length $ filter (\x -> sum (map (distance x) xs) < 10000) (getExpandedGrid (getBounds xs) (length xs))
