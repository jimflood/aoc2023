module Day1
    ( day1
    ) where

import Lib (slurpLines)
import Data.Char (isDigit)

digits :: String -> [Int]
digits = digits' []
    where
        digits' acc [] = reverse acc
        digits' acc (x : xs)
            | isDigit x = digits' ((read [x]::Int) : acc) xs
            | otherwise = digits' acc xs

digits2 :: String -> [Int]
digits2 = digits2' []
    where
        digits2' acc [] = reverse acc
        -- simple but effective
        digits2' acc ('o' : 'n' : 'e' : xs) = digits2' (1 : acc) ('n' : 'e' : xs)
        digits2' acc ('t' : 'w' : 'o' : xs) = digits2' (2 : acc) ('w' : 'o' : xs)
        digits2' acc ('t' : 'h' : 'r' : 'e' : 'e' : xs) = digits2' (3 : acc) ('h' : 'r' : 'e' : 'e' : xs)
        digits2' acc ('f' : 'o' : 'u' : 'r' : xs) = digits2' (4 : acc) ('o' : 'u' : 'r' : xs)
        digits2' acc ('f' : 'i' : 'v' : 'e' : xs) = digits2' (5 : acc) ('i' : 'v' : 'e' : xs)
        digits2' acc ('s' : 'i' : 'x' : xs) = digits2' (6 : acc) ('i' : 'x' : xs)
        digits2' acc ('s' : 'e' : 'v' : 'e' : 'n' : xs) = digits2' (7 : acc) ('e' : 'v' : 'e' : 'n' : xs)
        digits2' acc ('e' : 'i' : 'g' : 'h' : 't' : xs) = digits2' (8 : acc) ('i' : 'g' : 'h' : 't' : xs)
        digits2' acc ('n' : 'i' : 'n' : 'e' : xs) = digits2' (9 : acc) ('i' : 'n' : 'e' : xs)
        digits2' acc (x : xs)
            | isDigit x = digits2' ((read [x]::Int) : acc) xs
            | otherwise = digits2' acc xs

solve :: (String -> [Int]) -> [String] -> Int
solve f xs = sum $ map ( \ x -> (head x * 10) + (last x)) $ map f xs

day1 :: IO ()
day1 = do
    xs <- slurpLines "day1.txt"
    let answer1 = solve digits xs
    print $ "part 1: " ++ (show answer1)
    let answer2 = solve digits2 xs
    print $ "part 2: " ++ (show answer2)
