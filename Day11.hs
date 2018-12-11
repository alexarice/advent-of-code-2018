module Day11 where

import Data.Array

serialNumber :: Int
serialNumber = 5791

calculatePower :: Int -> Int -> Int
calculatePower x y = hundreds - 5 where
  hundreds = (intermediate `mod` 1000) `div` 100
  intermediate = a * rackId
  a = b + serialNumber
  b = rackId * y
  rackId = x + 10

powerArray :: Array (Int,Int) Int
powerArray = array ((1,1),(300,300)) [((x,y),calculatePower x y) | x <- [1..300], y<-[1..300]]

convolvedList :: Array (Int,Int) Int -> [(Int,(Int,Int))]
convolvedList a = map onePoint [(x,y) | x <- [2..299], y <- [2..299]] where
  onePoint (x,y) = (sum $ map (\(n1,n2) -> a!(x+n1,y+n2)) [(n1,n2) | n1 <- [-1..1], n2 <- [-1..1]], (x-1,y-1))

part1 :: (Int,Int)
part1 = let ls = convolvedList powerArray in snd (maximum ls)

-- We can do this by divide and conquer and dynamic programming.
-- If the size is even we can split it into 4 smaller squares.
-- Otherwise we can find it as the sum of 5 squares
combinedPowerArray :: Array (Int,Int) Int -> Array Int (Array (Int,Int) Int)
combinedPowerArray a = na where
  na = array (1,300) [(size, array ((1,1),(301-size,301-size)) [((x,y),f x y size) | x <- [1..301-size], y <- [1..301-size]]) | size <- [1..300]]
  getElement (x',y',s') = na!s'!(x',y')
  f x y size
    | size == 1 = a!(x,y)
    | size `mod` 2 == 0 = let s = size `div` 2
                          in sum $ map getElement [(x,y,s),(x,y+s,s),(x+s,y,s),(x+s,y+s,s)]
    | otherwise = sum (map getElement [(x,y,size-1),
                                       (x+1,y+1,size-1),
                                       (x+size-1,y,1),
                                       (x,y+size-1,1)]) - na!(size-2)!(x+1,y+1)

part2 :: (Int,(Int,Int,Int))
part2 = let a = combinedPowerArray powerArray
        in  maximum $ [(a!size!(x,y),(x,y,size)) | size <- [1..300], x<-[1..301-size], y <- [1..301-size]]
