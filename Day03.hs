module Day03 where

import Text.Parsec
import Data.List hiding (intersect)
import Day02

data Rectangle = Rectangle{ _elfId :: Int, _left :: Int, _top :: Int, _width :: Int, _height :: Int} deriving Show

getInt :: Parsec String st Int
getInt = read <$> many1 digit

parseData :: Parsec String st [Rectangle]
parseData = endBy line eol where
  eol = char '\n'
  line = do
    char '#'
    i <- getInt
    string " @ "
    l <- getInt
    char ','
    t <- getInt
    string ": "
    w <- getInt
    char 'x'
    h <- getInt
    return $ Rectangle i l t w h

contains :: Rectangle -> Int -> Int -> Bool
contains (Rectangle _ l t w h) x y = x >= l && x < l + w && y >= t && y < t + h

readParseFile :: IO [Rectangle]
readParseFile = do
  inputData <- readFile "./day3data.txt"
  case parse parseData "" inputData of
    Right x -> return x
    Left y -> error (show y)



part1 :: [Rectangle] -> Int
part1 = length . filter (>=2) . condense. sort . concatMap listPoints

listPoints :: Rectangle -> [(Int,Int)]
listPoints (Rectangle _ l t w h) = [(x,y) | x <- [l .. l+w-1], y <- [t .. t+h-1]]

intersect :: Rectangle -> Rectangle -> Bool
intersect (Rectangle _ l t w h) (Rectangle _ l' t' w' h') = intersect' l w l' w' && intersect' t h t' h' where
  intersect' a b a' b' = not (a+b <= a' || a' + b' <= a)

part2 :: [Rectangle] -> Int
part2 (r:rs) = if any (intersect r) rs then part2 $ part2Iterate [r] rs else _elfId r

part2Iterate :: [Rectangle] -> [Rectangle] -> [Rectangle]
part2Iterate [] xs = xs
part2Iterate (r:rs) ts = let (xs,ys) = partition (intersect r) ts in part2Iterate (rs ++ xs) ys
