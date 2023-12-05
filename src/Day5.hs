module Day5
    ( day5
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
-- import Debug.Trace (trace)

type ConversionTable = [(Int, Int, Int)] -- (destination, source, length)

type AlmanacMap = Map.Map String ConversionTable

type Range = (Int, Int) -- (start, length)

parseLines :: [String] -> ([Int], AlmanacMap)
parseLines xss = parse (chunk [] [] xss)
    where
        chunk acc r [] = reverse (reverse r : acc)
        chunk acc r ([] : xs) = chunk (reverse r : acc) [] xs
        chunk acc r (x : xs) = chunk acc (x : r) xs
        parse (a : bs) = (seeds a, Map.fromList (conversions bs))
        parse _ = error "Huh?"
        seeds a = map ( \ x -> read x::Int) $ drop 1 (splitOn " " (head a))
        conversions = map conversions'
        conversions' (y : ys) = (label y, conversions'' [] ys)
        conversions' _ = error "Huh??"
        conversions'' acc [] = acc
        conversions'' acc (b : bs) = conversions'' (range b : acc) bs
        range b = range' $  map ( \ x -> read x::Int) (splitOn " " b)
        range' [a, b, c] = (a, b, c)
        range' _ = error "huh?"
        label xs = head (splitOn " " xs)

-- path through the conversion tables from seed to location
path :: [String]
path = ["seed-to-soil", "soil-to-fertilizer", "fertilizer-to-water", "water-to-light", "light-to-temperature", "temperature-to-humidity", "humidity-to-location"]

locate :: AlmanacMap -> Int -> Int
locate am x = foldl ( \ a b -> convert (am Map.! b) a) x path

convert :: ConversionTable -> Int -> Int
convert [] x = x
convert ((a, b, c) : ds) x
    | x >= b && x < b + c = (x - b) + a
    | otherwise = convert ds x

solve1 :: AlmanacMap -> [Int] -> Int
solve1 am seeds = minimum $ map (locate am) seeds

-- reinterpret list of seeds as list of seed ranges instead
ranges :: [Int] -> [Range]
ranges = ranges' []
    where
        ranges' acc [] = reverse acc
        ranges' acc (a : b : cs) = ranges' ((a, b) : acc) cs
        ranges' _ _ = error "???"

locateRanges :: AlmanacMap -> [Range] -> [Range]
locateRanges am xs = foldl ( \ a b -> convertRanges (am Map.! b) a) xs path

convertRanges :: ConversionTable -> [Range] -> [Range]
convertRanges cm = concatMap (convertRanges' cm)
    where
        convertRanges' :: ConversionTable -> Range -> [Range] -- can be broken into subranges
        convertRanges' [] (x, y) = [(x, y)]
        convertRanges' ((a, b, c) : ds) (x, y)
            | x > (b + c - 1) || (x + y - 1) < b = convertRanges' ds (x, y) -- no overlap
            | x >= b && (x + y - 1) <= (b + c - 1) = [(x - (b - a), y)] -- completely overlap
            | x >= b = (x - (b - a), b + c - x) : convertRanges' ds (b + c, (x + y) - (b + c)) -- overlap on one side
            | (x + y - 1) < (b + c - 1) = (((x + y) - b) + a, (b + c) - (x + y)) : convertRanges' ds (x, b - x) -- overlap on the other side
            | otherwise = (a, c) : (convertRanges' ds (x, b - x) ++ convertRanges' ds (b + c, (x + y) - (b + c))) -- overlap only in the middle

solve2 :: AlmanacMap -> [Range] -> Int
solve2 am rs = minimum $ map fst $ locateRanges am rs

day5 :: IO ()
day5 = do
    (seeds, am) <- parseLines <$> slurpLines "day5.txt"
    let answer1 = solve1 am seeds
    print $ "part 1: " ++ show answer1
    let answer2 = solve2 am (ranges seeds)
    print $ "part 2: " ++ show answer2
