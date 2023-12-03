import Data.List (isPrefixOf)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)

-- PROBLEM 1:
-- Read digits from each line
-- Take the first and last digit and convert to a number in base 10 (duplicate if only 1 digit)
-- Sum the digits and return the number

-- PROBLEM 2:
-- Read digits and number words like "one" from each line
-- Perform the remaining steps from Problem 1

filterDigits1 :: String -> [Char]
filterDigits1 str
  | isPrefixOf "1" str = ['1'] ++ filterDigits1 (tail str)
  | isPrefixOf "2" str = ['2'] ++ filterDigits1 (tail str)
  | isPrefixOf "3" str = ['3'] ++ filterDigits1 (tail str)
  | isPrefixOf "4" str = ['4'] ++ filterDigits1 (tail str)
  | isPrefixOf "5" str = ['5'] ++ filterDigits1 (tail str)
  | isPrefixOf "6" str = ['6'] ++ filterDigits1 (tail str)
  | isPrefixOf "7" str = ['7'] ++ filterDigits1 (tail str)
  | isPrefixOf "8" str = ['8'] ++ filterDigits1 (tail str)
  | isPrefixOf "9" str = ['9'] ++ filterDigits1 (tail str)
  | isPrefixOf "0" str = ['0'] ++ filterDigits1 (tail str)
  | length str == 0 = []
  | otherwise = [] ++ filterDigits1 (tail str)

filterDigits2 :: String -> [Char]
filterDigits2 str
  | isPrefixOf "1" str = ['1'] ++ filterDigits2 (tail str)
  | isPrefixOf "2" str = ['2'] ++ filterDigits2 (tail str)
  | isPrefixOf "3" str = ['3'] ++ filterDigits2 (tail str)
  | isPrefixOf "4" str = ['4'] ++ filterDigits2 (tail str)
  | isPrefixOf "5" str = ['5'] ++ filterDigits2 (tail str)
  | isPrefixOf "6" str = ['6'] ++ filterDigits2 (tail str)
  | isPrefixOf "7" str = ['7'] ++ filterDigits2 (tail str)
  | isPrefixOf "8" str = ['8'] ++ filterDigits2 (tail str)
  | isPrefixOf "9" str = ['9'] ++ filterDigits2 (tail str)
  | isPrefixOf "0" str = ['0'] ++ filterDigits2 (tail str)
  | isPrefixOf "one" str = ['1'] ++ filterDigits2 (tail str)
  | isPrefixOf "two" str = ['2'] ++ filterDigits2 (tail str)
  | isPrefixOf "three" str = ['3'] ++ filterDigits2 (tail str)
  | isPrefixOf "four" str = ['4'] ++ filterDigits2 (tail str)
  | isPrefixOf "five" str = ['5'] ++ filterDigits2 (tail str)
  | isPrefixOf "six" str = ['6'] ++ filterDigits2 (tail str)
  | isPrefixOf "seven" str = ['7'] ++ filterDigits2 (tail str)
  | isPrefixOf "eight" str = ['8'] ++ filterDigits2 (tail str)
  | isPrefixOf "nine" str = ['9'] ++ filterDigits2 (tail str)
  | isPrefixOf "zero" str = ['0'] ++ filterDigits2 (tail str)
  | length str == 0 = []
  | otherwise = [] ++ filterDigits2 (tail str)

getFirstAndLastDigit :: [Char] -> Integer
getFirstAndLastDigit digit_chars = do
  read [head digit_chars, last digit_chars] :: Integer

solution1 :: [String] -> Integer
solution1 content = do
  let digits = map filterDigits1 content
  sum (map getFirstAndLastDigit digits)

solution2 :: [String] -> Integer
solution2 content = do
  let digits = map filterDigits2 content
  sum (map getFirstAndLastDigit digits)

example1 :: IO ()
example1 = do
  let content = unsafePerformIO . readFile $ "example1.txt"
  putStrLn "Example 1:"
  print (solution1 (lines content))

input1 :: IO ()
input1 = do
  let content = unsafePerformIO . readFile $ "input.txt"
  putStrLn "Input 1:"
  print (solution1 (lines content))

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
