import Data.Char (isSpace)
import Data.List (dropWhileEnd, find, isPrefixOf, transpose)
import Data.Map (Map, findWithDefault, fromList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)

-- Corresponding to the destination, source, length
type RangeMapping = (Integer, (Integer, Integer))

dst :: RangeMapping -> Integer
dst = fst

src :: RangeMapping -> Integer
src = fst . snd

len :: RangeMapping -> Integer
len = snd . snd

convert :: RangeMapping -> Integer -> Integer
convert r k = do
  let offset = k - src r
  dst r + offset

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

tuplify :: [a] -> (a, (a, a))
tuplify [x, y, z] = (x, (y, z))

inRange :: Integer -> RangeMapping -> Bool
inRange k r = do
  src r <= k && (src r + len r) > k

createMapping :: String -> [String] -> [RangeMapping]
createMapping name strs = do
  let lines = map trim (tail (takeWhile (not . null) (dropWhile (not . isPrefixOf name) strs)))
  map (tuplify . (map read . splitBy ' ')) lines

getValue :: [RangeMapping] -> Integer -> Integer
getValue m k = do
  let potential = find (inRange k) m
  -- trace (show potential) $ if isJust potential
  if isJust potential
    then convert (fromJust potential) k
    else k

increment :: (Integer, Integer) -> [Integer]
increment (start, len) =
  if len > 0
    then start : increment (start + 1, len - 1)
    else []

flatten :: [[a]] -> [a]
flatten = foldr1 (++)

pairwise :: [Integer] -> [(Integer, Integer)]
pairwise [] = []
pairwise l = (head l, head (tail l)) : pairwise (drop 2 l)

-- Boilerplate

solution1 :: [String] -> Integer
solution1 strs = do
  let seeds_to_plant = map read (splitBy ' ' (trim (last (splitBy ':' (head strs))))) :: [Integer]
      seed_to_soil = createMapping "seed-to-soil" strs
      soil_to_fertilizer = createMapping "soil-to-fertilizer" strs
      fertilizer_to_water = createMapping "fertilizer-to-water" strs
      water_to_light = createMapping "water-to-light" strs
      light_to_temperature = createMapping "light-to-temperature" strs
      temperature_to_humidity = createMapping "temperature-to-humidity" strs
      humidity_to_location = createMapping "humidity-to-location" strs
      location x = getValue humidity_to_location (getValue temperature_to_humidity (getValue light_to_temperature (getValue water_to_light (getValue fertilizer_to_water (getValue soil_to_fertilizer (getValue seed_to_soil x))))))
  minimum (map location seeds_to_plant)

solution2 :: [String] -> Integer
solution2 strs = do
  let seed_numbers = map read (splitBy ' ' (trim (last (splitBy ':' (head strs))))) :: [Integer]
      seeds_to_plant = flatten (map increment (pairwise seed_numbers))
      seed_to_soil = createMapping "seed-to-soil" strs
      soil_to_fertilizer = createMapping "soil-to-fertilizer" strs
      fertilizer_to_water = createMapping "fertilizer-to-water" strs
      water_to_light = createMapping "water-to-light" strs
      light_to_temperature = createMapping "light-to-temperature" strs
      temperature_to_humidity = createMapping "temperature-to-humidity" strs
      humidity_to_location = createMapping "humidity-to-location" strs
      location x = getValue humidity_to_location (getValue temperature_to_humidity (getValue light_to_temperature (getValue water_to_light (getValue fertilizer_to_water (getValue soil_to_fertilizer (getValue seed_to_soil x))))))
  minimum (map location seeds_to_plant)

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