{-# LANGUAGE TupleSections #-}

module Day07 where

import Text.Parsec
import Data.Array
import Data.List
import qualified Data.Array.ST as M
import Control.Monad.ST
import Control.Monad
import Data.Maybe
import Data.Char

type Transition = (Char,Char)

readParseFile :: IO [Transition]
readParseFile = do
  inputData <- readFile "./day7data.txt"
  case parse parseFile "" inputData of
    Right x -> return x
    Left y  -> error (show y)

parseFile :: Parsec String st [Transition]
parseFile = endBy parseLine endOfLine

parseLine :: Parsec String st Transition
parseLine = do
  _ <- string "Step "
  x <- upper
  _ <- string " must be finished before step "
  y <- upper
  _ <- string " can begin."
  return (x,y)

toArray :: [Transition] -> Array Char String
toArray ts = sort <$> accumArray t [] ('A','Z') ts where
  t xs x = x : xs

type MA s = M.STArray s Char (Int,String)

toMutableArray :: Array Char String -> ST s (MA s)
toMutableArray a = do
  a' <- M.thaw $ (0,) <$> a
  elements <- concatMap snd <$> M.getElems a'
  forM_ elements (incrementOne a')
  return a' where
    incrementOne :: MA s -> Char -> ST s ()
    incrementOne marray e = M.readArray marray e >>= \(n,xs) -> M.writeArray marray e (n+1,xs)

oneStep :: MA s -> String -> ST s String
oneStep _ [] = return []
oneStep a (x:xs) = do
  e <- M.readArray a x
  additions <- catMaybes <$> forM (snd e) (decrementOne a)
  es <- oneStep a (sort (xs ++ additions))
  return (x : es)

decrementOne :: MA s -> Char -> ST s (Maybe Char)
decrementOne a e = M.readArray a e >>= \(n,xs) -> M.writeArray a e (n-1,xs) >> return (if n == 1 then Just e else Nothing)

type Worker = Maybe (Char,Int)

part2Step :: MA s -> String -> [Worker] -> ST s Int
part2Step a todo workers = do
  let (t',w') = assignWork todo workers
  moveTime a t' w' where
    assignWork t [] = (t,[])
    assignWork [] w = ([], w)
    assignWork t (w@(Just _):ws)  = let (t',w') = assignWork t ws in (t',w:w')
    assignWork (x:xs)(Nothing:ws) = let (t',w') = assignWork xs ws in (t',Just (x,ord x - ord 'A' + 61) : w')
    moveTime :: MA s -> String -> [Worker] -> ST s Int
    moveTime marray t w = let workerTimes = map snd$catMaybes w
                     in if null workerTimes then return 0 else do
      let minTime = minimum workerTimes
          completed = map fst . filter (\(_,n) -> n == minTime) . catMaybes $ w
      done <- map snd <$> forM completed (M.readArray marray)
      additions <- sort .catMaybes <$> forM (concat done)(decrementOne marray)
      (+minTime) <$> part2Step marray (sort (t ++ additions)) (map (reduceWorker minTime) w)
    reduceWorker _ Nothing = Nothing
    reduceWorker n (Just (x,n')) = if n' == n then Nothing else Just (x,n'-n)

part1 :: [Transition] -> String
part1 ts = runST $ do
  a <- toMutableArray $ toArray ts
  initial <- map fst . filter (\x -> fst(snd x) == 0) <$> M.getAssocs a
  oneStep a initial

part2 :: [Transition] -> Int
part2 ts = runST $ do
  a <- toMutableArray $ toArray ts
  initial <- map fst . filter (\x -> fst(snd x) == 0) <$> M.getAssocs a
  part2Step a initial [Nothing,Nothing,Nothing,Nothing,Nothing]
