module Day17
    ( day17
    ) where

import Lib (slurpLines)
import Data.List (nub, partition, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Parallel.Strategies (parMap, rseq, rdeepseq)
import Debug.Trace (trace)

type Coordinate = (Int, Int)

type Grid = ((Int, Int), Map.Map Coordinate Int)

data Direction = North | South | East | West deriving (Eq, Ord, Show)

type Crucible = (Coordinate, Direction, Int)

type Cursor = (Crucible, Int, [(Crucible, Int)])

parseGrid :: [String] -> Grid
parseGrid css = ((length (head css), length css) , Map.fromList [((x, y), read [c]) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs])

start :: [Cursor]
start = [(((0, 0), East, 1), 0, []), (((0, 0), South, 1), 0, [])]

right :: Direction -> Direction
right North = East
right East = South
right South = West
right West = North

left :: Direction -> Direction
left North = West
left West = South
left South = East
left East = North

solve :: Grid -> Maybe (Int, Int) -> Int
solve ((mx, my), g) u = minimum $ map ( \ (_, b, _) -> b) $ solve' Map.empty ([], []) start
    where
        solve' :: Map.Map Crucible Int -> ([Cursor], [Cursor]) -> [Cursor] -> [Cursor]
        solve' _ (a, []) [] = a
        solve' s (a, b) [] = solve' s (a, []) b
        solve' s (a, b) (p : ps) = solve'' $ catMaybes (parMap rseq (heat . prune . step) (split p)) -- (heat . prune . step)
            where
                goal :: Cursor -> Bool
                goal (((x, y), _, n), _, _) = (x, y) == (mx - 1, my - 1) && n >= maybe 0 fst u -- n + 1 ???
                solve'' :: [Cursor] -> [Cursor]
                solve'' xs = solve''' (foldl ( \ mm (c, h, _) -> Map.insert c h mm) s xs) (partition goal xs)
                solve''' :: Map.Map (Coordinate, Direction, Int) Int -> ([Cursor], [Cursor]) -> [Cursor]
                solve''' newS (xs, ys) = solve' newS (a ++ xs, b ++ ys) ps
                split (((x, y), d, n), h, t)
                    | n < maybe (-1) fst u = [(((x, y), d, n), h, t)]
                    | otherwise = [(((x, y), right d, 0), h, t), (((x, y), left d, 0), h, t), (((x, y), d, n), h, t)]
                step :: Cursor -> Cursor
                step (((x, y), North, n), h, t) = (((x, y - 1), North, n + 1), h, t)
                step (((x, y), South, n), h, t) = (((x, y + 1), South, n + 1), h, t)
                step (((x, y), East, n), h, t) = (((x + 1, y), East, n + 1), h, t)
                step (((x, y), West, n), h, t) = (((x - 1, y), West, n + 1), h, t)
                prune :: Cursor -> Maybe Cursor
                prune (((x, y), d, n), h, t)
                    | n > maybe 3 snd u = Nothing
                    | (x, y) `Map.notMember` g = Nothing
                    | any ( \ (((x0, y0), d0, n0), h0) -> s Map.! ((x0, y0), d0, n0) < h0) t = Nothing
                    | otherwise = Just (((x, y), d, n), h, t)
                heat :: Maybe Cursor -> Maybe Cursor
                heat (Just (((x, y), d, n), h, t))
                    | ((x, y), d, n) `Map.notMember` s || s Map.! ((x, y), d, n) > (h + (g Map.! (x, y))) = Just (((x, y), d, n), h + (g Map.! (x, y)), (((x, y), d, n), h) : t)  -- TODO update t elsewhere
                    | otherwise = Nothing
                heat Nothing = Nothing

day17 :: IO ()
day17 = do
    grid <- parseGrid <$> slurpLines "day17.txt"
    -- putStr (draw grid)
    let answer1 = solve grid Nothing
    print $ "part 1: " ++ show answer1
    let answer2 = solve grid (Just (4, 10))
    print $ "part 2: " ++ show answer2
