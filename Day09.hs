module Day09 where

import qualified Data.Array as A
import Data.List

players :: Int
players = 478

lastMarble :: Int
lastMarble = 71240

data CircularList a = CList {before :: [a], after :: [a]} deriving Show

current :: CircularList a -> a
current (CList _ (x : _)) = x
current _ = error "current called on empty clist"

-- Add element before current element and make it the current element
addElement :: a -> CircularList a -> CircularList a
addElement x (CList as bs) = CList as (x:bs)

-- Make the element clockwise of the current element the current element
forward :: CircularList a -> CircularList a
forward (CList as (a : b : bs)) = CList (a : as) (b : bs)
forward (CList as [b]) = CList [] (reverse (b : as))
forward _ = error "forward called with no current element"

-- Make the element anticlockwise of the current element the current element
backwards :: CircularList a -> CircularList a
backwards (CList (a : as) bs) = CList as (a : bs)
backwards (CList [] (b : bs)) = backwards $ CList (reverse bs) [b]
backwards _ = error "backwards called with no current element"

-- Pop current element leaving the element clockwise of it being the current one
pop :: CircularList a -> (a, CircularList a)
pop (CList as (b : a : bs)) = (b,CList as (a : bs))
pop (CList (a : as) [b]) = (b, CList [] (reverse (a : as)))
pop _ = error "pop called with no current element"

initialList :: CircularList Int
initialList = CList [] [0]

data GameState = GS {circle :: CircularList Int, toPlayNext :: Int, scorings :: [(Int,Int)]} deriving Show

apply :: Int -> (a -> a) -> a -> a
apply 0 _ c = c
apply n f c = apply (n-1) f (f c)

incrementPlayNext :: Int -> Int
incrementPlayNext n
  | n == players = 1
  | otherwise = n+1

initialGS :: GameState
initialGS = GS initialList 1 []

playTurn :: GameState -> Int -> GameState
playTurn (GS c n ss) m
  | m `mod` 23 == 0 = let (a,c') = pop (apply 7 backwards c) in GS c' (incrementPlayNext n) ((n,m):(n,a):ss)
  | otherwise = GS (addElement m . forward . forward $ c) (incrementPlayNext n) ss



part1 :: Int
part1 = maximum . A.elems $ A.accumArray (+) 0 (1,players) (scorings endState) where
  endState = foldl playTurn initialGS [1..lastMarble]

-- This was originally running into problems caused by haskell lazyness. It turns
-- out evaluating the scores from highest to lowest was really bad. Also foldl
-- should almost never be used. The whole thing could probably be improved further
-- by forcing strictness in more places though this was sufficient.
part2 :: Int
part2 = maximum . A.elems $ A.accumArray (+) 0 (1,players) (reverse $ scorings endState) where
  endState = foldl' playTurn initialGS [1..lastMarble*100]
