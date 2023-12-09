module Day9
    ( day9
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOn)

type Sequence = [Int]

parseLines :: [String] -> [Sequence]
parseLines = map parse
    where
        parse xs = map ( \ x -> read x::Int) (splitOn " " xs)

solve :: ([Sequence] -> Int) -> Sequence -> Int
solve f = solve' []
    where
        solve' acc xs
            | all (==0) xs = f acc
            | otherwise = solve' (xs : acc) (less xs)

less :: Sequence -> Sequence
less = less' []
    where
        less' acc (x : y : ys) = less' ((y - x) : acc) (y : ys)
        less' acc _ = reverse acc

more :: [Sequence] -> Int
more = more' 0
    where
        more' n [] = n
        more' n (x : xs) = more' (n + last x) xs

more2 :: [Sequence] -> Int
more2 = more2' 0
    where
        more2' n [] = n
        more2' n (x : xs) = more2' (head x - n) xs

day9 :: IO ()
day9 = do
    input <- parseLines <$> slurpLines "day9.txt"
    let answer1 = sum $ map (solve more) input
    print $ "part 1: " ++ show answer1
    let answer2 = sum $ map (solve more2) input
    print $ "part 2: " ++ show answer2
