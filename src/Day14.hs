module Day14
    ( day14
    ) where

import Lib (slurpLines)
import qualified Data.Map as Map

type Coordinate = (Int, Int)

type Grid = ((Int, Int), Map.Map Coordinate Char)

parseGrid :: [String] -> Grid
parseGrid css = ((length (head css), length css) , Map.fromList [((x, y), c) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs])

tilt :: (Coordinate -> Coordinate) -> Grid -> Grid
tilt f = tilt'
    where
        tilt' :: Grid -> Grid
        tilt' (z, g) = (z, roll)
            where
                roll = foldl roll' g [k | (k, v) <- if f (0, 0) > (0, 0) then reverse (Map.toList g) else Map.toList g, v == 'O']
                roll' a b
                    | Map.lookup (f b) a == Just '.' = roll' (Map.insert (f b) 'O' (Map.insert b '.' a)) (f b)
                    | otherwise = a

north :: Coordinate -> Coordinate
north (x, y) = (x, y - 1)

south :: Coordinate -> Coordinate
south (x, y) = (x, y + 1)

east :: Coordinate -> Coordinate
east (x, y) = (x + 1, y)

west :: Coordinate -> Coordinate
west (x, y) = (x - 1, y)

load :: Grid -> Int
load ((_, my), g) = sum [my - y | ((_, y), v) <- Map.toList g, v == 'O']

spin :: Grid -> Grid
spin g = (tilt east . tilt south . tilt west . tilt north) g

findCycle :: Grid -> ((Int, Int), Grid)
findCycle = findCycle' 0 Map.empty
    where
        findCycle' :: Int -> Map.Map Grid Int -> Grid -> ((Int, Int), Grid)
        findCycle' n m g = findCycle'' (Map.lookup g m)
            where
                findCycle'' (Just x) = ((x, n), g)
                findCycle'' Nothing = findCycle' (n + 1) (Map.insert g n m) (spin g)

solve1 :: Grid -> Int
solve1 g = load $ tilt north g

solve2 :: Grid -> Int
solve2 g = load $ solve2' $ findCycle g
    where
        solve2' ((x, y), ng) = foldl ( \a _ -> spin a) ng [1..(1000000000 - x) `rem` (y - x)]

day14 :: IO ()
day14 = do
    grid <- parseGrid <$> slurpLines "day14.txt"
    let answer1 = solve1 grid
    print $ "part 1: " ++ show answer1
    let answer2 = solve2 grid
    print $ "part 2: " ++ show answer2
