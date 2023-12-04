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

getNumbers :: String -> [Integer]
getNumbers str = do
    map read (filter (not . null) (splitBy ' ' (trim str)))

-- Boilerplate

solution1 :: [String] -> Integer
solution1 strs = do
    let winning_numbers str = getNumbers (last (splitBy ':' (head (splitBy '|' (trim str)))))
    let card_numbers str = getNumbers (last (splitBy '|' (trim str)))
    let all_winning_nums = map winning_numbers strs
    let all_card_nums = map card_numbers strs
    let matching_numbers = zipWith intersect all_winning_nums all_card_nums
    sum (map ((\x -> 2^(x-1)) . length) (filter (not . null) matching_numbers))

solution2 :: [String] -> Integer
solution2 strs = do
  0

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