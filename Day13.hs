module Day13 where

import Prelude
import Data.Array
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import Data.Maybe

type Tracks = Array (Int,Int) Tile

data Tile = None | ForwardSlash | BackSlash | Intersection deriving Show

data Direction = L | R | U | D deriving (Eq,Ord,Show)

data Train = Train {steps :: Int, position :: (Int,Int), direction :: (Direction, Int)} deriving (Eq,Show)

instance Ord Train where
  t1 <= t2 = (steps t1, (snd.position) t1, (fst.position) t1) <= (steps t2, (snd.position) t2, (fst.position) t2)

data InputData = InputData Tracks [Train] deriving Show

readParseFile :: IO InputData
readParseFile = do
  inputFile <- T.readFile "./day13data.txt"
  let rows = map T.unpack . filter (not . T.null) $ T.split (=='\n') inputFile
  let zippedRows = map (\(n,r) -> zip [(n',n) | n' <- [0..]] r) $ zip [0..] rows
  let trains = getTrains (concat zippedRows)
  let tracks = array ((0,0),(length (head zippedRows) - 1, length zippedRows - 1)) $ concatMap (map convert) zippedRows
  return $ InputData tracks trains where
    getTrains [] = []
    getTrains (((x,y),c):xs)
      | c == '^' = Train 0 (x,y) (U,0) : getTrains xs
      | c == 'v' = Train 0 (x,y) (D,0) : getTrains xs
      | c == '<' = Train 0 (x,y) (L,0) : getTrains xs
      | c == '>' = Train 0 (x,y) (R,0) : getTrains xs
      | otherwise = getTrains xs
    convert ((x,y),c) = ((x,y),convert' c)
    convert' '\\' = BackSlash
    convert' '/' = ForwardSlash
    convert' '+' = Intersection
    convert' _ = None

part1 :: InputData -> (Int,Int)
part1 (InputData tracks trains) = oneStep (sort trains) where
  oneStep (t:ts) = let mt = move tracks t in
    fromMaybe (oneStep (sort (mt:ts))) $ collision mt ts

collision :: Train -> [Train] -> Maybe (Int,Int)
collision mt ts = if position mt `elem` map position ts then Just (position mt) else Nothing

move :: Tracks -> Train -> Train
move tracks (Train n (x,y) d) = rotate $ Train (n+1) (case d of
                                                   (U,_) -> (x,y-1)
                                                   (D,_) -> (x,y+1)
                                                   (L,_) -> (x-1,y)
                                                   (R,_) -> (x+1,y)
                                                ) d where
  rotate (Train n (x,y) d) = let tile = tracks!(x,y) in
    Train n (x,y) $ case (tile,d) of
                      (None,d') -> d'
                      (ForwardSlash,(U,i)) -> (R,i)
                      (ForwardSlash,(D,i)) -> (L,i)
                      (ForwardSlash,(L,i)) -> (D,i)
                      (ForwardSlash,(R,i)) -> (U,i)
                      (BackSlash,(U,i)) -> (L,i)
                      (BackSlash,(D,i)) -> (R,i)
                      (BackSlash,(L,i)) -> (U,i)
                      (BackSlash,(R,i)) -> (D,i)
                      (Intersection, (d',0)) -> (turnLeft d',1)
                      (Intersection, (d',1)) -> (d',2)
                      (Intersection, (d',2)) -> (turnRight d',0)
  turnLeft U = L
  turnLeft L = D
  turnLeft D = R
  turnLeft R = U
  turnRight = turnLeft . turnLeft . turnLeft

part2 :: InputData -> (Int,Int)
part2 (InputData tracks trains) = oneStep (sort trains) where
  oneStep [t] = position (move tracks t) -- the remaining cart will be a tick behind
  oneStep (t:ts) = let mt = move tracks t in
    case collision mt ts of
      Nothing -> oneStep (sort (mt:ts))
      Just x -> oneStep (filter ((/=x).position) ts)
