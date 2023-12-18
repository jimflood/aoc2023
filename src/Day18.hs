module Day18
    ( day18
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOn)

data Direction = DigUp | DigDown | DigLeft | DigRight deriving (Eq, Ord, Show)

data DigInstruction = DI { dir :: Direction
                         , len :: Int
                         , color :: String } deriving (Eq, Ord, Show)

type Coordinate = (Int, Int)

parseLines :: [String] -> [DigInstruction]
parseLines css = map parse $ map ( \ cs -> splitOn " " cs) css
    where
        parse [x, y, z] = DI (toDir x) (read y) ((tail . init) z)
            where
                toDir :: String -> Direction
                toDir "U" = DigUp
                toDir "D" = DigDown
                toDir "L" = DigLeft
                toDir "R" = DigRight
                toDir _ = error "???"
        parse _ = error"???"

-- Shoelace formula
-- cf. Mathologer youtube video: https://www.youtube.com/watch?v=0KjG8Pg6LGk
area :: [Coordinate] -> Int
area xs = area' 0 xs
    where
        area' acc ((x1, y1) : (x2, y2) : cs) = area' (((x1 * y2) - (x2 * y1)) + acc) ((x2, y2) : cs)
        area' acc _ = acc `div` 2

-- Turning the corners affects the sides of the polygon slightly:
corners :: [DigInstruction] -> [(Int, Int) -> Int -> (Int, Int)]
corners dis = corners' ([last dis] ++ dis ++ [head dis])
    where
        corners' :: [DigInstruction] -> [(Int, Int) -> Int -> (Int, Int)]
        corners' xs = corners'' [] $ map dir xs
        corners'' :: [(Int, Int) -> Int -> (Int, Int)] -> [Direction] -> [(Int, Int) -> Int -> (Int, Int)]
        corners'' acc (a : b : c : ds) = corners'' ((delta a b c) : acc) (b : c : ds)
        corners'' acc _ = reverse acc
        --  CLOCKWISE +1
        delta DigRight DigDown DigLeft = \ (x, y) n -> (x, y + n + 1)
        delta DigLeft DigUp DigRight = \ (x, y) n -> (x, y - n - 1)
        delta DigUp DigRight DigDown = \ (x, y) n -> (x + n + 1, y)
        delta DigDown DigLeft DigUp = \ (x, y) n -> (x - n - 1, y)
        -- COUNTERCLOCKWISE -1
        delta DigRight DigUp DigLeft = \ (x, y) n -> (x, y - n + 1)
        delta DigLeft DigDown DigRight = \ (x, y) n -> (x, y + n - 1)
        delta DigUp DigLeft DigDown = \ (x, y) n -> (x - n + 1, y)
        delta DigDown DigRight DigUp = \ (x, y) n -> (x + n - 1, y)
        -- Zig-Zag = 0
        delta a DigRight c | a == c && a /= DigRight = \ (x, y) n -> (x + n, y)
        delta a DigLeft c | a == c && a /= DigLeft = \ (x, y) n -> (x - n, y)
        delta a DigUp c | a == c && a /= DigUp = \ (x, y) n -> (x, y - n)
        delta a DigDown c | a == c && a /= DigDown = \ (x, y) n -> (x, y + n)
        delta i j k = error ("Cannot occur: " ++ show i ++ " " ++ show j ++ " " ++ show k)

-- Calculate vertices of polygon, then area of that polygon.
solve :: [DigInstruction] -> Int
solve input = solve' $ zip input (corners input)
    where
        solve' ts = solve'' $ reverse $ foldl ( \ a (di, f) -> (f (head a) (len di)) : a) [(0, 0)] ts
        solve'' vs = area vs

-- Fix up the instructions for part 2.
redo :: [DigInstruction] -> [DigInstruction]
redo dis = map ( \ di -> redo' (color di)) dis
    where
        redo' xs = redo'' (read ("0x" ++ (take  5 (drop 1 xs)))::Int) (drop 6 xs)
            where
                redo'' n "0" = DI DigRight n xs
                redo'' n "1" = DI DigDown n xs
                redo'' n "2" = DI DigLeft n xs
                redo'' n "3" = DI DigUp n xs
                redo'' q p = error ("Ruh-roh: " ++ show q ++ " " ++ p)

day18 :: IO ()
day18 = do
    input <- parseLines <$> slurpLines "day18.txt"
    let answer1 = solve input
    print $ "part 1: " ++ show answer1
    let answer2 = solve (redo input)
    print $ "part 2: " ++ show answer2
