import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import GHC.Float (rationalToFloat, int2Float)
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

-- Copied from https://stackoverflow.com/a/9722949
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

parseNumbers1 :: String -> [Integer]
parseNumbers1 str = map read (filter (not . null) (splitBy ' ' (trim (last (splitBy ':' str)))))

parseNumbers2 :: String -> Integer
parseNumbers2 str = read (filter (/=' ') (last (splitBy ':' str)))

getRounds1 :: (String, String) -> [(Integer, Integer)]
getRounds1 (times, distances) = zip (parseNumbers1 times) (parseNumbers1 distances)

getRounds2 :: (String, String) -> (Integer, Integer)
getRounds2 (times, distances) = (parseNumbers2 times, parseNumbers2 distances)

computeRoots :: Integer -> Integer -> (Float, Float)
computeRoots time distance = (b1, r2)
    where
        isint x = x == fromInteger (round x)
        root1 a b c = ((-b) + sqrt (b**2 - 4 * a * c)) / (2*a)
        root2 a b c = ((-b) - sqrt (b**2 - 4 * a * c)) / (2*a)
        time' = int2Float (fromIntegral time)
        distance' = int2Float (fromIntegral distance)
        r1 = root1 1 (-time') distance'
        r2 = root2 1 (-time') distance'
        b1 = if (not . isint) r1
            then r1
            else r1-1

-- Boilerplate

solution1 :: IO String -> IO Integer
solution1 io = do
    content <- io
    let
        strs = (head (lines content), head (tail (lines content)))
        rounds = getRounds1 strs
        roots = map (mapTuple floor . uncurry computeRoots) rounds
        num_elems = map (uncurry (-)) roots
        p = product num_elems
    return p

solution2 :: IO String -> IO Integer
solution2 io = do
    content <- io
    let
        strs = (head (lines content), head (tail (lines content)))
        rounds = getRounds2 strs
        roots = uncurry computeRoots rounds
        num_elems = (uncurry (-) . mapTuple floor) roots
    print rounds
    print roots
    return num_elems

example1 :: IO ()
example1 = do
  let content = readFile "example.txt"
  solution <- solution1 content
  putStrLn ("Example 1:\t" ++ show solution)

input1 :: IO ()
input1 = do
  let content = readFile "input.txt"
  solution <- solution1 content
  putStrLn ("Input 1:\t" ++ show solution)

example2 :: IO ()
example2 = do
  let content = readFile "example.txt"
  solution <- solution2 content
  putStrLn ("Example 2:\t" ++ show solution)

input2 :: IO ()
input2 = do
  let content = readFile "input.txt"
  solution <- solution2 content
  putStrLn ("Input 2:\t" ++ show solution)

main :: IO ()
main = do
  example1
  input1
  example2
  input2