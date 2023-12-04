import GHC.IO (unsafePerformIO)
import Data.List (dropWhileEnd, intersect)
import Data.Char (isSpace)
import Debug.Trace (trace)

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


-- Problem 1
-- Create a set of the winning numbers
-- Create a set of the numbers for the card
-- Get the union of the two sets
-- Let N be the cardinality of the set
-- Compute 2^(N-1) for each card
-- Sum the values

-- Problem 2
-- Let K be the number of card
-- Let multipliers = [1] * K
-- For each card L
--   Create a set of the winning numbers
--   Create a set of the numbers for the card
--   Get the union of the two sets
--   Let N be the cardinality of the set
--   Let M = N * multipliers[L]
--   Add M to the next N multipliers 
-- sum multipliers

getNumbers :: String -> [Integer]
getNumbers str = do
    map read (filter (not . null) (splitBy ' ' (trim str)))

safeTail :: [a] -> [a]
safeTail = drop 1



countMatchingNumbers :: String -> Integer
countMatchingNumbers str = do
    let winning_numbers = getNumbers (last (splitBy ':' (head (splitBy '|' (trim str)))))
    let card_numbers = getNumbers (last (splitBy '|' (trim str)))
    toInteger (length (winning_numbers `intersect` card_numbers))

computeWonCards :: [String] -> [Integer] -> [Integer]
computeWonCards strs multipliers = do
    let card_score = countMatchingNumbers (head strs)
    let added_values = replicate (fromInteger card_score) (head multipliers)
    let updated_multipliers = zipWith (+) added_values (tail multipliers)
    let new_multipliers = updated_multipliers ++ drop (length updated_multipliers + 1) multipliers
    if not (null strs)
        then head multipliers : computeWonCards (tail strs) new_multipliers
        else []

-- Boilerplate

solution1 :: [String] -> Integer
solution1 strs = do
    sum (map (\x -> 2^(x-1)) (filter (> 0) (map countMatchingNumbers strs)))

solution2 :: [String] -> Integer
solution2 strs = do
    let multipliers = replicate (length strs) 1
    sum (computeWonCards strs multipliers)

example1 :: IO ()
example1 = do
  let content = unsafePerformIO . readFile $ "example.txt"
  putStrLn "Example 1:"
  print (solution1 (lines content))

input1 :: IO ()
input1 = do
  let content = unsafePerformIO . readFile $ "input.txt"
  putStrLn "Input 1:"
  print (solution1 (lines content))

example2 :: IO ()
example2 = do
  let content = unsafePerformIO . readFile $ "example.txt"
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