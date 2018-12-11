module Day04 where

import           Data.List
import qualified Data.Map.Strict    as M
import           Data.Time.Calendar
import           Text.Parsec
import           Text.Printf

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

dateParser :: Parsec String st Day
dateParser = do
  y <- read <$> many1 digit
  _ <- char '-'
  m <- read <$> many1 digit
  _ <- char '-'
  d <- read <$> many1 digit
  case fromGregorianValid y m d of
    Nothing         -> parserFail (printf "date \"%d,%d,%d\" invalid" y m d)
    Just parsedDate -> return parsedDate

data Time = Time {hours :: Int, minutes :: Int} deriving (Eq,Ord,Show)

timeParser :: Parsec String st Time
timeParser = do
  h <- read <$> many1 digit
  _ <- char ':'
  m <- read <$> many1 digit
  return $ Time h m

data Timestamp = Timestamp {date :: Day, time :: Time, event :: Event} deriving (Eq,Ord,Show)
data Event = WakeUp | FallAsleep | StartShift Int deriving (Eq, Ord, Show)

eventParser :: Parsec String st Event
eventParser = wakeUp <|> fallAsleep <|> startShift where
  wakeUp = string "wakes up" >> return WakeUp
  fallAsleep = string "falls asleep" >> return FallAsleep
  startShift = do
    _ <- string "Guard #"
    number <- read <$> many1 digit
    _ <- string " begins shift"
    return $ StartShift number

timestampParser :: Parsec String st Timestamp
timestampParser = do
  _ <- char '['
  d <- dateParser
  _ <- space
  t <- timeParser
  _ <- string "] "
  Timestamp d t <$> eventParser

fileParser :: Parsec String st [Timestamp]
fileParser = endBy timestampParser endOfLine

readParseFile :: IO [Timestamp]
readParseFile = do
  inputData <- readFile "./day4data.txt"
  case parse fileParser "" inputData of
    Right x -> return x
    Left y  -> error (show y)

data SleepSpan = SleepSpan {guardId :: Int, startMinute :: Int, endMinute :: Int} deriving (Eq,Ord,Show)

getSleepSpans :: [Timestamp] -> [SleepSpan]
getSleepSpans = gather 0 . sort where
  gather _ (Timestamp _ _ (StartShift n):ts) = gather n ts
  gather n (Timestamp _ t1 FallAsleep:Timestamp _ t2 WakeUp:ts) = SleepSpan n (minutes t1) (minutes t2) : gather n ts
  gather _ [] = []

part1 :: [SleepSpan] -> Int
part1 ss = let (i,m) = getSolution in i*m where
  getSolution = (sleepyGuard, getMinute (filter (\s -> guardId s == sleepyGuard) ss))
  sleepyGuard = snd . maximum . map swap .M.assocs $ sleepTable
  sleepTable = foldr (\(SleepSpan i t1 t2) table -> M.insertWith (+) i (t2-t1) table) M.empty ss
  getMinute ssi = snd . maximum $ [(getOccur n ssi,n) | n <- [0..59]]
  getOccur n ss = length $ filter (contains n) ss
  contains n (SleepSpan _ t1 t2) = n >= t1 && n < t2

part2 :: [SleepSpan] -> Int
part2 ss = let (i,m) = getSolution in i*m where
  getSolution = snd . maximum . map swap . M.assocs $ sleepTable
  sleepTable = foldr (M.alter incOrStart) M.empty keys
  incOrStart (Just x) = Just (x+1)
  incOrStart Nothing = Just 1
  keys = [(i,m) | SleepSpan i t1 t2 <- ss, m <- [t1.. (t2-1)]]
