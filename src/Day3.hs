module Day3
    ( day3
    ) where

import Lib (slurpLines)
import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Map as Map

type Coordinate = (Int, Int)

type Grid = Map.Map Coordinate Char

type Number = ([Coordinate], Int)

type GearRatio = Int

parseGrid :: [String] -> Grid
parseGrid css = Map.fromList [((x, y), c) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs]

parseNumbers :: [String] -> [Number]
parseNumbers xss = concatMap ( \ (y, xs) -> nos [] [] 0 y xs) $ zip [0..] xss
    where
        nos :: [([(Int, Int)], Int)] -> [((Int, Int), Char)] -> Int -> Int -> String -> [([(Int, Int)], Int)]
        nos acc [] _ _ [] = acc
        nos acc [] x y (c : cs)
            | isDigit c = nos acc [((x, y), c)] (x + 1) y cs
            | otherwise = nos acc [] (x + 1) y cs
        nos acc ds _ _ [] = numberfy ds : acc
        nos acc ds x y (c : cs)
            | isDigit c = nos acc (((x, y), c) : ds) (x + 1) y cs
            | otherwise = nos (numberfy ds : acc) [] (x + 1) y cs
        numberfy ds = (map fst ds, read (reverse (map snd ds))::Int)

adjacent :: Number -> [Coordinate]
adjacent n = nub $ concatMap neighbors (fst n)
    where
        neighbors (x, y) = [(nx, ny) | nx <- [(x - 1)..(x + 1)], ny <- [(y - 1)..(y + 1)], (nx, ny) /= (x, y)]

isPartNumber :: Grid -> ([(Int, Int)], Int) -> Bool
isPartNumber g n = any isSym (adjacent n)
    where
        isSym (x, y) = isSym' $ Map.lookup (x, y) g
        isSym' (Just '.') = False
        isSym' (Just c) = not (isDigit c)
        isSym' Nothing = False

partNumbers :: Grid -> [Number] -> [Int]
partNumbers g ns = map snd $ filter (isPartNumber g) ns 
 
gears :: Grid -> [Number] -> [GearRatio]
gears g ns = map snd $ Map.toList $ Map.mapMaybeWithKey gear g
    where
        gear :: Coordinate -> Char -> Maybe GearRatio
        gear (x, y) '*' = gear' $ filter ( \ n -> (x, y) `elem` adjacent n) ns
        gear (_, _) _ = Nothing
        gear' ys
            | length ys == 2 = Just (product (map snd ys))
            | otherwise = Nothing

day3 :: IO ()
day3 = do
    input <- slurpLines "day3.txt"
    let g = parseGrid input
    let ns = parseNumbers input
    let answer1 = sum $ partNumbers g ns 
    print $ "part 1: " ++ show answer1
    let answer2 = sum $ gears g ns
    print $ "part 2: " ++ show answer2
