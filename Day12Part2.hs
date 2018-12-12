{-# LANGUAGE TemplateHaskell #-}

module Day12Part2 where

import Prelude hiding (null,length,replicate,drop,take)
import Control.Comonad
import Data.Sequence hiding (zip)
import Data.Foldable (toList, foldl')
import Control.Lens hiding ((:>),(:<),(<|),(|>))
import Data.Array
import Text.Parsec
import Control.DeepSeq

-- This will not work for part2 as we will calculate too many needless calculation
-- Therefore make a localised version
-- This contains information about where the pots are (we should keep this sorted)
-- It also has endpoints after which everything should be false

data Pots a = Pots {_before :: Seq a,
                    _focus :: a,
                    _focusPos :: Int,
                    _after :: Seq a} deriving Show

makeLenses ''Pots

instance Functor Pots where
  fmap f (Pots xs y n zs) = Pots (fmap f xs) (f y) n (fmap f zs)

getLeft :: Pots a -> Seq (Pots a)
getLeft p@(Pots xs _ _ _)
  | null xs = empty
  | otherwise = left (oneLeft p)

left :: Pots a -> Seq (Pots a)
left p@(Pots xs _ _ _)
  | null xs = singleton p
  | otherwise = left (oneLeft p) |> p

oneLeft :: Pots a -> Pots a
oneLeft (Pots xs y n zs) = g (viewr xs) where
  g (s :> x) = Pots s x (n-1) (y <| zs)

getRight :: Pots a -> Seq (Pots a)
getRight p@(Pots _ _ _ zs)
  | null zs = empty
  | otherwise = right (oneRight p)

right :: Pots a -> Seq (Pots a)
right p@(Pots _ _ _ zs)
  | null zs = singleton p
  | otherwise = p <| right (oneRight p)

oneRight :: Pots a -> Pots a
oneRight (Pots xs y n zs) = g (viewl zs) where
  g (z :< s) = Pots (xs |> y) z (n+1) s

instance Comonad Pots where
  extract p = p^.focus
  duplicate p@(Pots _ _ n _) = Pots (getLeft p) p n (getRight p)

type Neighbours = (Bool,Bool,Bool,Bool,Bool)
data InputData = InputData (Pots Bool) (Array Neighbours Bool)

readParseFile :: IO InputData
readParseFile = do
  inputData <- readFile "./day12data.txt"
  case parse fileParser "" inputData of
    Right x -> return x
    Left y  -> error (show y)

fileParser :: Parsec String st InputData
fileParser = do
  p <- initialParser
  _ <- endOfLine
  _ <- endOfLine
  InputData p <$> arrayParser

isPot :: Parsec String st Bool
isPot = (=='#') <$> oneOf ".#"

initialParser :: Parsec String st (Pots Bool)
initialParser = do
  _ <- string "initial state: "
  (x:xs) <- many1 isPot
  return $ Pots empty x 0 (fromList xs)

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

numberFalseLeft :: Seq Bool -> Int
numberFalseLeft = length . takeWhileL not

numberFalseRight :: Seq Bool -> Int
numberFalseRight = length . takeWhileR not

trimLeft :: Seq Bool -> Seq Bool
trimLeft s = let n = numberFalseLeft s in
  if n <= 2 then replicate (2-n) False >< s else drop (n - 2) s

trimRight :: Seq Bool -> Seq Bool
trimRight s = let n = numberFalseRight s in
  if n <= 2 then s >< replicate (2-n) False else take (length s - n + 2) s

trim :: Pots Bool -> Pots Bool
trim = over before trimLeft . over after trimRight

step :: Array Neighbours Bool -> Pots Bool -> Bool
step ar (Pots xs c _ zs) = ar!(a,b,c,d,e) where
  (a,b) = getLast2 (viewr xs)
  (d,e) = getFirst2 (viewl zs)
  getLast2 EmptyR = (False,False)
  getLast2 (as :> b') = (getLast (viewr as), b')
  getLast EmptyR = False
  getLast (_ :> a') = a'
  getFirst2 EmptyL = (False,False)
  getFirst2 (d' :< es) = (d',getFirst (viewl es))
  getFirst EmptyL = False
  getFirst (e' :< _) = e'

oneStep :: Array Neighbours Bool -> Pots Bool -> Pots Bool
oneStep a = trim . extend (step a)

instance NFData a => NFData (Pots a) where
  rnf (Pots xs y n zs) = y `seq` n `seq` xs `deepseq` zs `deepseq` ()

sumIterate :: Int -> InputData -> Int
sumIterate n (InputData p a) = sumPot endPot where
  sumPot (Pots xs _ _ zs) = sum $  map (\(m,x) -> if x then m else 0) $ zip [-length xs.. -1] (toList xs) ++ zip [1..] (toList zs)
  endPot = foldl' (\r f -> force (f r)) (trim p) (replicate n (oneStep a))

part1 = sumIterate 20

part2 :: InputData -> Int
part2 i = let p200 = sumIterate 200 i
              inc = p200 - sumIterate 199 i
          in p200 + inc * (50000000000 - 200)
