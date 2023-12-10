module Day10
    ( day10
    ) where

import Lib (slurpLines)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing)

type Coordinate = (Int, Int)

type Grid = Map.Map Coordinate Char

type FloodMap = Map.Map Coordinate (Maybe Int)

parseGrid :: [String] -> Grid
parseGrid css = Map.fromList [((x * 3 + 1, y * 3 + 1), c) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs]

inflate :: Grid -> FloodMap
inflate g = Map.fromList $ map ( \ x -> (x, Nothing)) $ concatMap inflate' (Map.toList g)
    where
        inflate' :: (Coordinate, Char) -> [Coordinate]
        inflate' (p, 'S') = inflate'' p ['n', 's', 'e', 'w']
        inflate' (p, '|') = inflate'' p ['n', 's']
        inflate' (p, '-') = inflate'' p ['e', 'w']
        inflate' (p, 'L') = inflate'' p ['n', 'e']
        inflate' (p, 'J') = inflate'' p ['n', 'w']
        inflate' (p, '7') = inflate'' p ['s', 'w']
        inflate' (p, 'F') = inflate'' p ['s', 'e']
        inflate' _ = []
        inflate'' :: Coordinate -> [Char] -> [Coordinate]
        inflate'' (x, y) ds = (x, y) : map inflate''' ds
            where
                inflate''' 'n' = (x, y - 1)
                inflate''' 's' = (x, y + 1)
                inflate''' 'e' = (x + 1, y)
                inflate''' 'w' = (x - 1, y)
                inflate''' _ = error "???"

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) = [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]

flood :: FloodMap -> [Coordinate] -> FloodMap
flood = flood' 0 []
    where
        flood' _ [] m [] = m
        flood' n acc m [] = flood' (n + 1) [] m (nub acc)
        flood' n acc m (p : ps)
            | Map.member p m && isNothing (m Map.! p) = flood' n (neighbors p ++ acc) (Map.insert p (Just n) m) ps
            | otherwise = flood' n acc m ps

solve1 :: FloodMap -> Int
solve1 m = maximum (catMaybes (Map.elems m)) `div` 3

-- draw :: FloodMap -> String
-- draw m = concatMap draw' [0..snd (bounds m)]
--     where
--         draw' y = concatMap draw'' [0..fst (bounds m)] ++ "\n"
--             where
--                 draw'' x
--                     | Map.notMember (x, y) m && (x `mod` 3 == 1) && (y `mod` 3 == 1) = "@"
--                     | Map.notMember (x, y) m = "."
--                     | otherwise = draw''' (m Map.! (x, y))
--                 draw''' Nothing = "*"
--                 draw''' (Just x)
--                     | x `mod` 3 == 0 = show ((x `div` 3) `mod` 10)
--                     | otherwise = "#"

bounds :: FloodMap -> (Int, Int)
bounds m = (bounds' fst, bounds' snd)
    where
        bounds' f = maximum (map f (Map.keys m))

edges :: FloodMap -> [Coordinate]
edges m = edges' (bounds m)
    where
        edges' (mx, my) = northern ++ southern ++ eastern ++ western
            where
                northern = [(x, 0) | x <- [0..mx]]
                southern = [(x, my) | x <- [0..mx]]
                eastern = [(mx, y) | y <- [0..my]]
                western = [(0, y) | y <- [0..my]]

open :: FloodMap -> FloodMap
open m = open' (bounds m)
    where
        open' (mx, my) = open'' [(x, y) | x <- [0..mx], y <- [0..my]]
        open'' = foldl open''' m
        open''' a b
            | Map.notMember b a = Map.insert b Nothing a
            | otherwise = a

deflate :: Map.Map Coordinate (Maybe Int) -> Map.Map Coordinate (Maybe Int)
deflate m = deflate' (bounds m)
    where
        deflate' (mx, my) = deflate'' [(x, y) | x <- [0..mx `div` 3], y <- [0..my `div` 3]]
        deflate'' = foldl ( \ a (x, y) -> Map.insert (x, y) (m Map.! (x * 3 + 1, y * 3 + 1)) a) Map.empty

start :: Grid -> Coordinate
start g = head $ Map.keys $ Map.filter (== 'S') g

solve2 :: FloodMap -> Int
solve2 m = length $ filter isNothing (Map.elems (deflate m))

day10 :: IO ()
day10 = do
    grid <- parseGrid <$> slurpLines "day10.txt"
    let s = start grid
    let m = inflate grid
    let mf = flood m [s]
    let answer1 = solve1 mf
    print $ "part 1: " ++ show answer1
    let mf2 = flood (open mf) (edges mf)
    let answer2 = solve2 mf2
    print $ "part 2: " ++ show answer2
