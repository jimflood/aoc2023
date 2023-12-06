module Day6
    ( day6
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOn)

type Race = (Int, Int)

parseLines :: [String] -> [Race]
parseLines (a : b : _) = zip (parse a) (parse b)
    where
        parse xs = map (\ x -> read x::Int) (tail (filter (not . null) (splitOn " " xs)))
parseLines _ = error "???"

graph :: Int -> [Int]
graph n = map ( \ x -> x * (n - x)) [0..n]

solve :: [Race] -> Int
solve rs = product $ map (length . solve') rs
    where
        solve' r = filter ( \x -> x  > snd r) $ graph (fst r)

smush :: [Race] -> Race
smush rs = (smush' fst, smush' snd)
    where
        smush' f = read (concatMap (show . f) rs) :: Int

day6 :: IO ()
day6 = do
    races <- parseLines <$> slurpLines "day6.txt"
    let answer1 = solve races
    print $ "part 1: " ++ show answer1
    let answer2 = solve [smush races]
    print $ "part 2: " ++ show answer2
