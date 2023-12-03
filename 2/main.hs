import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, isSuffixOf)
import Data.Map (Map, findWithDefault, fromList, keys)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)

-- PROBLEM 1
-- Game id sum = 0
-- For each game
--   For each round
--     Compare num dice vs constraints
--     If num dice per category < constraints
--       Game id sum + Game id

data Color = Red | Green | Blue deriving (Read, Show, Enum, Eq, Ord)

type NumDice = Map Color Integer

constraint1 :: NumDice
constraint1 = fromList [(Red, 12), (Green, 13), (Blue, 14)]

-- Copied from https://stackoverflow.com/a/7569301
splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

-- Copied from https://stackoverflow.com/a/38283069
trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

parseDiceColor :: String -> Maybe Color
parseDiceColor str
  | isSuffixOf "red" (map toLower str) = Just Red
  | isSuffixOf "green" (map toLower str) = Just Green
  | isSuffixOf "blue" (map toLower str) = Just Blue
  | otherwise = Nothing

computeGameId :: String -> Integer
computeGameId game_str = do
  let game_delc = head (splitBy ':' game_str)
  read (last (splitBy ' ' game_delc)) :: Integer

computeDieCount :: String -> Maybe (Color, Integer)
computeDieCount str = do
  let trimmed = trim str
  let color = parseDiceColor trimmed
  case color of
    Just c -> Just (c, read (head (splitBy ' ' trimmed)))
    Nothing -> Nothing

parseRound :: String -> NumDice
parseRound round = do
  let trimmed = trim round
  let dice = splitBy ',' trimmed
  let counts = mapMaybe computeDieCount dice
  fromList counts

evaluateRound :: NumDice -> NumDice -> Bool
evaluateRound constraint round = do
  let getOrZero m color = findWithDefault 0 color m
  let invalidAmount color = getOrZero constraint color < getOrZero round color
  not (any invalidAmount (keys constraint))

evaluateGame :: NumDice -> String -> Integer
evaluateGame constraint content = do
  let trimmed = trim content
  let rounds = splitBy ';' (last (splitBy ':' trimmed))
  let evaluate = evaluateRound constraint
  if all (evaluate . parseRound) rounds
    then computeGameId content
    else 0

-- Boilerplate

solution1 :: NumDice -> [String] -> Integer
solution1 constraint content = do
  let eval = evaluateGame constraint
  sum (map eval content)

solution2 :: [String] -> Integer
solution2 content = do
  0

example1 :: IO ()
example1 = do
  let content = unsafePerformIO . readFile $ "example1.txt"
  putStrLn "Example 1:"
  print (solution1 constraint1 (lines content))

input1 :: IO ()
input1 = do
  let content = unsafePerformIO . readFile $ "input.txt"
  putStrLn "Input 1:"
  print (solution1 constraint1 (lines content))

example2 :: IO ()
example2 = do
  let content = unsafePerformIO . readFile $ "example2.txt"
  putStrLn "Example 2:"
  print (solution2 (lines content))

input2 :: IO ()
input2 = do
  let content = unsafePerformIO . readFile $ "input.txt"
  putStrLn "Input 2:"
  print (solution2 (lines content))

main :: IO ()
main = do
  example1
  input1
  example2
  input2