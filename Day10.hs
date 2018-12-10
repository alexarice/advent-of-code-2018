module Day10 where

import Text.Parsec

data Point = Point {position :: (Int,Int), velocity :: (Int,Int)} deriving Show

readParseFile :: IO [Point]
readParseFile = do
  inputData <- readFile "./day10data.txt"
  case parse fileParser "" (filter (/=' ') inputData) of
    Right x -> return x
    Left y  -> error (show y)


fileParser :: Parsec String st [Point]
fileParser = endBy line endOfLine where
  line = do
    _ <- string "position=<"
    x <- signedInt
    _ <- string ","
    y <- signedInt
    _ <- string ">velocity=<"
    xv <- signedInt
    _ <- string ","
    yv <- signedInt
    _ <- string ">"
    return $ Point (x,y) (xv,yv)
  signedInt = (char '-' >> negate <$> getInt) <|> getInt
  getInt = read <$> many1 digit

updatePosition :: Point -> Point
updatePosition (Point (x,y) (xv,yv)) = Point (x+xv,y+yv) (xv,yv)

getBounds :: [Point] -> (Int,Int,Int,Int)
getBounds (Point (x,y) _:p:ps) = let (a,b,c,d) = getBounds (p:ps) in (min a x,max b x, min c y, max d y)
getBounds [Point (x,y) _] = (x,x,y,y)
getBounds [] = error "List is empty"

getBoundFitness :: [Point] -> Int
getBoundFitness ps = let (a,b,c,d) = getBounds ps in b - a + d - c

picture :: [Point] -> String
picture ps = let (a,b,c,d) = getBounds ps
                 printLine n ps' = map (whichPrint n ps') [a..b]
                 whichPrint n ps' m = if any (\(Point p _) -> p == (m,n)) ps' then '#' else '.'
             in
  foldr (\x y -> x ++ '\n' : y) "\n" [printLine n (filter (\(Point (_,y) _) -> y == n) ps) | n <- [c..d]]

printPictures :: [Point] -> [String]
printPictures = printPictures' 0 where
  printPictures' :: Int -> [Point] -> [String]
  printPictures' n ps
    | n >= 5 = []
    | n < 5 = picture ps : if getBoundFitness newPoints > getBoundFitness ps then
        printPictures' (n+1) newPoints else printPictures' 0 newPoints where
        newPoints = map updatePosition ps

part1 :: [Point] -> IO [()]
part1 = mapM putStrLn . (\xs -> drop (length xs - 10) xs) . printPictures

part2 :: [Point] -> IO [()]
part2 ps = mapM putStrLn . map (\(ss,n) -> show n ++ "\n" ++ ss).(\xs -> drop (length xs - 10) xs) $ zip (printPictures ps) [0..]
