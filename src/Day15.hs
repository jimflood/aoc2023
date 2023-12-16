module Day15
    ( day15
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as Map
import Data.Char (ord)
import Data.List (findIndex)

data Operation = AddLens String Int | RemoveLens String deriving Show

type LensBox = [(String, Int)]

type BoxMap = Map.Map Int LensBox

parseLines :: [String] -> [String]
parseLines = concatMap (splitOn ",")

hash :: String -> Int
hash = hash' 0
    where
        hash' :: Int -> String -> Int
        hash' acc [] = acc
        hash' acc (x : xs) = hash' (((acc + ord x) * 17) `mod` 256) xs

operations :: [String] -> [Operation]
operations = map operations'
    where
        operations' x = operations'' $ splitOneOf "-=" x
            where
                operations'' [a, ""] | '-' `elem` x = RemoveLens a
                operations'' [a, b]  | '=' `elem` x = AddLens a (read b)
                operations'' _ = error "Huh?"

step :: BoxMap -> Operation -> BoxMap
step m (AddLens label n) = Map.alter addLens (hash label) m
    where
        addLens :: Maybe LensBox -> Maybe LensBox
        addLens Nothing = error "???"
        addLens (Just [(x, _)]) | x == label = Just [(label, n)]
        addLens (Just ts) = addLens' $ findIndex ( \ (x, _) -> x == label) ts
            where
                addLens' Nothing = Just (ts ++ [(label, n)])
                addLens' (Just i) = Just (take i ts ++ [(label, n)] ++ drop (i + 1) ts)
step m (RemoveLens label) = Map.alter removeLens (hash label) m
    where
        removeLens :: Maybe LensBox -> Maybe LensBox
        removeLens Nothing = error "???"
        removeLens (Just [(x, _)]) | x == label = Just []
        removeLens (Just ts) = removeLens' $ findIndex ( \ (x, _) -> x == label) ts
            where
                removeLens' Nothing = Just ts
                removeLens' (Just 0) = Just (tail ts)
                removeLens' (Just i) = Just (take i ts ++ drop (i + 1) ts)

emptyMap :: BoxMap
emptyMap =  Map.fromList [(k, []) | k <-[0..255]]

power :: (Int, LensBox) -> [Int]
power (i, xs) = map power' (zip [1..] xs)
    where
        power' :: (Int, (String, Int)) -> Int
        power' (s, (_, n)) = (i + 1) * s * n

solve1 :: [String] -> Int
solve1 xs = sum $ map hash xs

solve2 :: [Operation] -> Int
solve2 xs = sum $ map sum $ map power $ Map.toList $ foldl step emptyMap xs

day15 :: IO ()
day15 = do
    input <- parseLines <$> slurpLines "day15.txt"
    let answer1 = solve1 input
    print $ "part 1: " ++ show answer1
    let answer2 = solve2  (operations input)
    print $ "part 2: " ++ show answer2
