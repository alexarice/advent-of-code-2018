module Day12 where

import Text.Parsec
import Control.Comonad
import Data.Array
import Control.Arrow hiding (left,right)


-- Pots is defined as follows:
-- The first argument represents pots to the left
-- This should be a infinite lists
-- Same for pots to the right
-- The center represents the focused pot.
-- The integer represents the position
data Pots a = Pots [a] a Int [a]

instance Functor Pots where
  fmap f (Pots a b n c) = Pots (map f a) (f b) n (map f c)

left :: Pots a -> Pots a
left (Pots (x:xs) y n zs) = Pots xs x (n-1) (y:zs)
left _ = error "Pots not infinite"

right :: Pots a -> Pots a
right (Pots xs y n (z:zs)) = Pots (y:xs) z (n+1) zs
right _ = error "Pots not infinite"

instance Comonad Pots where
  extract (Pots _ x _ _) = x
  duplicate p@(Pots _ _ n _) = Pots (tail $ iterate left p) p n (tail $ iterate right p)

replicateA :: (Arrow a) => Int -> a b b -> a b b
replicateA 0 _ = arr id
replicateA n f = f >>> replicateA (n-1) f

type Neighbours = (Bool,Bool,Bool,Bool,Bool)

fromArray :: Array Neighbours Bool -> Cokleisli Pots Bool Bool
fromArray ar = Cokleisli f where
  f (Pots (b:a:_) c _ (d:e:_)) = ar!(a,b,c,d,e)
  f _ = False
  -- Add extra cases for part2

data InputData = InputData {pots :: Pots Bool, number :: Int, steps :: Array Neighbours Bool}

readParseFile :: IO InputData
readParseFile = do
  inputData <- readFile "./day12data.txt"
  case parse fileParser "" inputData of
    Right x -> return x
    Left y  -> error (show y)

fileParser :: Parsec String st InputData
fileParser = do
  (p,n) <- initialParser
  _ <- endOfLine
  _ <- endOfLine
  InputData p n <$> arrayParser

isPot :: Parsec String st Bool
isPot = (=='#') <$> oneOf ".#"

initialParser :: Parsec String st (Pots Bool, Int)
initialParser = do
  _ <- string "initial state: "
  (x:xs) <- many1 isPot
  return (Pots falses x 0 (xs++falses),length xs) where
    falses = repeat False

arrayParser :: Parsec String st (Array Neighbours Bool)
arrayParser = do
  ms <- endBy mapParser endOfLine
  return $ array ((False,False,False,False,False),(True,True,True,True,True)) ms

mapParser :: Parsec String st (Neighbours, Bool)
mapParser = do
  (a:b:c:d:e:_) <- many1 isPot
  _ <- string " => "
  f <- isPot
  return ((a,b,c,d,e),f)

part1 :: Int -> InputData -> Int
part1 steps (InputData p n s) = sumPots endPots where
  sumPots (Pots xs y _ zs) = sum $ take (2*steps) xs ++ y : take (n + 2*steps) zs
  endPots :: Pots Int
  endPots = extend (runCokleisli arrow) p
  arrow :: Cokleisli Pots Bool Int
  arrow = replicateA steps (fromArray s) &&& getPos >>> arr combine
  getPos = Cokleisli (\(Pots _ _ pos _) -> pos)
  combine (False,_) = 0
  combine (True,pos) = pos
