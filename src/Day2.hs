module Day2
    ( day2
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as Map

type Color = String

type Grab = Map.Map Color Int

type Id = Int

type Game = (Id, [Grab])

parseLines :: [String] -> [Game]
parseLines s = map parse s
    where
        parse x = parse' (splitOn ":" x)
        parse' [a, b] = parse'' (splitOn " " a) (splitOn ";" b)
        parse' _ = error "Huh?"
        parse'' [_, a] xs = parseGame [] (read a::Int) xs
        parse'' _ _ = error "Say again?"
        parseGame acc gid [] = (gid, reverse acc)
        parseGame acc gid (x : xs) = parseGame ((grab x) : acc) gid xs
        grab x = grab' [] (splitOneOf " ," x)
        grab' acc [] = Map.fromList acc
        grab' acc (_ : v : k : ys) = grab' ((k, read v::Int) : acc) ys
        grab' _ _ = error "I'm confused"

model :: Grab
model = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

empty :: Grab
empty = Map.fromList [("red", 0), ("green", 0), ("blue", 0)]

possible :: Game -> Bool
possible (_, gs) = all possible' gs
    where
        possible' g = all possible'' (Map.toList g)
        possible'' (k, v) = v <= (model Map.! k)

solve :: [Game] -> Int
solve xs = sum $ map fst $ filter possible xs

fewest :: Game -> Grab
fewest (_, gs) = foldl (\ a b -> Map.unionWith max b a) empty gs

power :: Game -> Int
power x = Map.foldr (*) 1 (fewest x)

solve2 :: [Game] -> Int
solve2 xs = sum (map power xs)

day2 :: IO ()
day2 = do
    xs <- parseLines <$> slurpLines "day2.txt"
    let answer1 = solve xs
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve2 xs
    print $ "part 2: " ++ (show answer2)
