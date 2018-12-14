module Day14 where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.List
import Debug.Trace
import qualified Data.Sequence as S
import Data.Char

puzzleInput :: Int
puzzleInput = 320851

part1slow :: Int -> String
part1slow n = concatMap (show . (a V.!)) [n..n+9] where
  l = [3,7] ++ makeRecipes 0 1 2
  a = V.generate (n+10) (l!!)
  makeRecipes x y number =
                           let x' = a V.! x
                               y' = a V.! y
                               z = x' + y'
                               n' = number + 1
                               n'' = number + 2
                           in
    if z <= 9 then  z : makeRecipes (move x x' n') (move y y' n') n'
    else 1 : z `mod` 10 : makeRecipes (move x x' n'') (move y y' n'') n''
  move x x' number = (x + x' + 1) `mod` number

data DependentList a = DL a a (a -> a -> (a,DependentList a))

arrayFromDependentList :: Int -> DependentList Int -> V.Vector Int
arrayFromDependentList n lstart = V.create $ do
  am <- VM.new n
  fill am 0 lstart
  return am where
    fill am m (DL x y l) = if m==n then return () else do
      x' <- VM.read am x
      y' <- VM.read am y
      let (x,l') = l x' y'
      VM.write am m x
      fill am (m+1) l'

part1 :: Int -> String
part1 n = concatMap (show . (afinal (n+10) V.!)) [n..n+9]

afinal :: Int -> V.Vector Int
afinal n = arrayFromDependentList (n) (DL 0 0 $ const.const (3,DL 0 0 $ const.const (7, makeRecipes 0 1 2))) where
  makeRecipes x y number = DL x y $ \x' y' ->
    let
      z = x' + y'
      n' = number + 1
      n'' = number + 2
    in
    if z <= 9 then  (z, makeRecipes (move x x' n') (move y y' n') n')
    else (1, DL 0 0 $ const . const (z `mod` 10, makeRecipes (move x x' n'') (move y y' n'') n''))
  move x x' number = (x + x' + 1) `mod` number

part2 :: [Int] -> Int
part2 xs = searchFor 0 infiniteList where
  infiniteList = constructInfinite 10
  constructInfinite n = traceShow n $ V.toList (afinal n) ++ drop n (constructInfinite (2*n))
  searchFor n ys = if xs `isPrefixOf` ys then n else n+1 `seq` searchFor (n+1) (tail ys)

part2seq :: String -> Int
part2seq xs = searchFor 0 infiniteseq where
  searchFor n ys = if xs `isPrefixOf` ys then n else (if ((n+1) `mod` 10000 == 0) then traceShow n else id) ((n+1) `seq` searchFor (n+1) (tail ys))
  infiniteseq :: String
  infiniteseq = "37" ++ makeRecipes 0 1 (S.fromList "37") where
    makeRecipes x y ss = let x' = digitToInt $ S.index ss x
                             y' = digitToInt $ S.index ss y
                             z = x' + y'
                             n' = S.length ss + 1
                             n'' = S.length ss + 2
                         in
      if z <= 9 then intToDigit z : makeRecipes (move x x' n') (move y y' n') (ss S.|> intToDigit z)
      else '1' : intToDigit (z-10) : makeRecipes (move x x' n'') (move y y' n'') (ss S.|> '1' S.|> intToDigit (z-10))
    move x x' number = (x + x' + 1) `mod` number
