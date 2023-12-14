module Day13
    ( day13
    ) where

import Lib (slurpLines)
import qualified Data.Map as Map
import Data.List ((\\), nub)

type Coordinate = (Int, Int)

type Grid = ((Int, Int), Map.Map Coordinate Char)

parseLines :: [String] -> [Grid]
parseLines = parseLines' [] []
    where
        parseLines' :: [Grid] -> [String] -> [String] -> [Grid]
        parseLines' acc g [] = reverse (parseGrid (reverse g) : acc)
        parseLines' acc g ("" : xs) = parseLines' (parseGrid (reverse g) : acc) [] xs
        parseLines' acc g (x : xs) = parseLines' acc (x : g) xs

parseGrid :: [String] -> Grid
parseGrid css = ((length (head css), length css) , Map.fromList [((x, y), c) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs])

scanH :: Grid -> [Int]
scanH ((mx, my), g) = [1..mx - 1] \\ map snd (filter fst $ map hit $ filter sieve gen)
    where
        gen = [(n, ((x, y), (2 * n - 1 - x, y))) | n <- [1..mx - 1], x <- [0..mx - 2], y <- [0..my - 1]]
        sieve (_, ((x1, _), (x2, _))) = x1 < x2 && x2 >= 0 && x2 < mx
        hit (n, (p1, p2)) = (g Map.! p1 /= g Map.! p2, n)

scanV :: Grid -> [Int]
scanV ((mx, my), g) = [1..my - 1] \\ map snd (filter fst $ map hit $ filter sieve gen)
    where
        gen = [(n, ((x, y), (x, 2 * n - 1 - y))) | n <- [1..my - 1], y <- [0..my - 2], x <- [0..mx - 1]]
        sieve (_, ((_, y1), (_, y2))) = y1 < y2 && y2 >= 0 && y2 < my
        hit (n, (p1, p2)) = (g Map.! p1 /= g Map.! p2, n)

scan :: Grid -> [Int]
scan g = nub $ scanH g ++ map (100 *) (scanV g)

solve1 :: [Grid] -> Int
solve1 gs = sum $ map (extract . scan) gs
    where
        extract :: [Int] -> Int
        extract [x] = x
        extract _ = error "Ruh-roh"

altScan :: Grid -> [Int]
altScan (b, g) = nub $ concatMap (scan . unsmudge . fst) $ Map.toList g
    where
        unsmudge k = (b, Map.adjust unsmudge' k g)
            where
                unsmudge' '.' = '#'
                unsmudge' '#' = '.'
                unsmudge' _ = error "Oops"

solve2 :: [Grid] -> Int
solve2 gs = sum $ map solve2' gs
    where
        solve2' :: Grid -> Int
        solve2' g = discern (solve1 [g]) (altScan g)
        discern x ys = head (ys \\ [x])

day13 :: IO ()
day13 = do
    input <- parseLines <$> slurpLines "day13.txt"
    let answer1 = solve1 input
    print $ "part 1: " ++ show answer1
    let answer2 = solve2 input
    print $ "part 2: " ++ show answer2
