module Day11
    ( day11
    ) where

import Lib (slurpLines)
import qualified Data.Map as Map

type Coordinate = (Int, Int)

type Grid = Map.Map Coordinate (Char, (Int, Int))

parseGrid :: [String] -> Grid
parseGrid css = Map.fromList [((x, y), (c, (0, 0))) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs]

maxX :: Grid -> Int
maxX g = maximum (map fst (Map.keys g))

maxY :: Grid -> Int
maxY g = maximum (map snd (Map.keys g))

expand :: Int -> Grid -> Grid
expand scale = expandRight scale . expandDown scale

expandRight :: Int -> Grid -> Grid
expandRight scale g = expandRight' g (maxX g)
    where
        expandRight' acc x
            | x == -1 = acc
            | all (\ p -> fst (acc Map.! p) == '.') [(x, y) | y <- [0..maxY g]] = expandRight' (shiftRight scale acc x) (x - 1)
            | otherwise = expandRight' acc (x - 1)

shiftRight ::Int -> Grid -> Int -> Grid
shiftRight scale g px = Map.mapWithKey shiftRight' g
    where
        shiftRight' :: Coordinate -> (Char, (Int, Int)) -> (Char, (Int, Int))
        shiftRight' (x, _) (c, (dx, dy))
            | x > px = (c, (dx + scale - 1, dy))
            | otherwise = (c, (dx, dy))

expandDown :: Int -> Grid -> Grid
expandDown scale g = expandDown' g (maxY g)
    where
        expandDown' acc y
            | y == -1 = acc
            | all (\ p -> fst (acc Map.! p) == '.') [(x, y) | x <- [0..maxX g]] = expandDown' (shiftDown scale acc y) (y - 1)
            | otherwise = expandDown' acc (y - 1)

shiftDown :: Int -> Grid -> Int -> Grid
shiftDown scale g py = Map.mapWithKey shiftDown' g
    where
        shiftDown' (_, y) (c, (dx, dy))
            | y > py = (c, (dx, dy + scale - 1))
            | otherwise = (c, (dx, dy))

stars :: Grid -> [Coordinate]
stars g = Map.keys $ Map.filter ((== '#') . fst) g

pairings :: [Coordinate] -> [(Coordinate, Coordinate)]
pairings xs = [(a, b) | a <- xs, b <- xs, a /= b, a < b]

distance :: Grid -> (Coordinate, Coordinate) -> Int
distance g ((x1, y1), (x2, y2)) = distance' (g Map.! (x1, y1)) (g Map.! (x2, y2))
    where
        distance' (_, (dx1, dy1)) (_, (dx2, dy2)) = abs ((x1 + dx1) - (x2 + dx2)) + abs ((y1 + dy1) - (y2 + dy2))

solve :: Grid -> Int
solve g = sum $ map (distance g) $ pairings (stars g)

day11 :: IO ()
day11 = do
    grid <- parseGrid <$> slurpLines "day11.txt"
    let answer1 = solve $ expand 1 grid
    print $ "part 1: " ++ show answer1
    let answer2 = solve $ expand 1000000 grid
    print $ "part 2: " ++ show answer2
