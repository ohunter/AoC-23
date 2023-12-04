import GHC.IO (unsafePerformIO)
import Debug.Trace (trace)

-- Helpers
type Point = (Integer, Integer)
type NumberPosition = (Point, Integer)

symbols :: [Char]
symbols = "*#+$"

digits :: [Char]
digits = "1234567890"

enumerate :: [b] -> [(Integer, b)]
enumerate = zip [0..]

-- Problem 1

getCharPositions :: (Integer, String, String) -> [Point]
getCharPositions (line_num, line, symbols) = do
    let symbolIndices line = map fst (filter (\(_, x) -> x `elem` symbols) (enumerate line))
    let symbolsForLine = map (line_num,) (symbolIndices line)
    trace (show symbolsForLine) symbolsForLine

findCharPositions :: ([String], [Char]) -> [[Point]]
findCharPositions (lines, chars) = do
    let getPoints (line_num, line) = getCharPositions (line_num, line, chars)
    let points = map getPoints (enumerate lines)
    trace (show points) points

combineDigitsOnLine :: [Point] -> [NumberPosition]
combineDigitsOnLine digit_positions = do
    let 
    []

getNumberPositions :: [String] -> [[NumberPosition]]
getNumberPositions lines = do
  let digit_positions = findCharPositions (lines, digits)
  trace (show digit_positions) map combineDigitsOnLine digit_positions

-- Boilerplate

solution1 :: [String] -> Integer
solution1 strs = do
  let numberPositions = getNumberPositions strs
  let symbolPositions = findCharPositions (strs, symbols)
  trace (show numberPositions) 0

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