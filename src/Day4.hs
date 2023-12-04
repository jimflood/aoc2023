module Day4
    ( day4
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOneOf)
import qualified Data.Map as Map

type Card = (Int, ([Int], [Int]))

parseLines :: [String] -> [Card]
parseLines = map parseLine
    where
        parseLine :: String -> (Int, ([Int], [Int]))
        parseLine xs = parseLine' $ filter (/= []) (splitOneOf " :" xs)
        parseLine' (_ : y : ys) = ((read y::Int), parseFst [] ys)
        parseLine' _ = error "nope"
        parseFst acc ("|" : ys) = parseSnd acc [] ys
        parseFst acc (y : ys) = parseFst ((read y::Int) : acc) ys
        parseFst _ _ = error "also nope"
        parseSnd a acc [] = (reverse a, reverse acc)
        parseSnd a acc (y : ys) = parseSnd a ((read y::Int) : acc) ys

-- count of matches
count :: Card -> Int
count (_, (xs, ys)) = length [y | y <- ys, y `elem` xs]

points :: Card -> Int
points c = points' (count c)
    where
        points' 0 = 0
        points' n = 2^(n - 1)

solve1 :: [Card] -> Int
solve1 cs = sum $ map points cs

solve2 :: [Card] -> Int
solve2 cs = sum $ Map.elems $ solve2' originals counts (map fst cs)
    where
        originals = Map.fromList (map (\ x -> (fst x, 1)) cs) -- one of each
        counts = Map.fromList (map (\ x -> (fst x, count x)) cs)
        solve2' acc _ [] = acc
        solve2' acc m (x : xs) = solve2' (copies (m Map.! x)) m xs
            where
                copies n = copies' acc [(x + 1)..(x + n)]
                    where
                        copies' g [] = g
                        -- snowball accumulated counts based on prior accumulated counts
                        copies' g (y : ys) = copies' (Map.adjust (+ (g Map.! x)) y g) ys

day4 :: IO ()
day4 = do
    input <- parseLines <$> slurpLines "day4.txt"
    let answer1 = solve1 input
    print $ "part 1: " ++ show answer1
    let answer2 = solve2 input
    print $ "part 2: " ++ show answer2
