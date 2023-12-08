module Day7
    ( day7
    ) where

import Data.Ord (Down(Down))
import Lib (slurpLines)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.List (sort, group, sortOn, sortBy, maximumBy)

type Hand = String

type Bid = Int

type HandOrder = [Int]

type CardOrder = [Int]

parseLines :: [String] -> [(Hand, Bid)]
parseLines = map (parse . splitOn " ")
    where
        parse [a, b] = (a, read b::Int)
        parse _ = error "Huh?"

classify :: (Hand, Bid) -> ((Hand, Bid), (HandOrder, CardOrder))
classify (h, n) = ((h, n), (handOrder h, cardOrder h))

-- grouped and sorted counts, for example: [4,1] four of a kind, [3, 2] full house, [2, 1, 1, 1] pair, etc.
handOrder :: Hand -> HandOrder
handOrder h = map length $ (sortOn (Down . length) . group . sort) h

cardOrder :: Hand -> CardOrder
cardOrder = map card'
    where
        card' 'A' = 14
        card' 'K' = 13
        card' 'Q' = 12
        card' 'J' = 11
        card' 'T' = 10
        card' n
            | isDigit n = read [n]::Int
            | otherwise = error "Nope"

compareHands :: ((Hand, Bid), (HandOrder, CardOrder)) -> ((Hand, Bid), (HandOrder, CardOrder)) -> Ordering
compareHands (_, (_, [])) (_, (_, [])) = EQ
compareHands (x, ([], b1 : b1s)) (y, ([], b2 :b2s))
    | b1 < b2 = LT
    | b1 > b2 = GT
    | b1 == b2 = compareHands (x, ([], b1s)) (y, ([], b2s))
compareHands (x, (a1 : a1s, b1)) (y, (a2 : a2s, b2))
    | a1 < a2 = LT
    | a1 > a2 = GT
    | a1 == a2 = compareHands (x, (a1s, b1)) (y, (a2s, b2))
compareHands _ _ = error "???"

-- all possible hands replacing Jokers
wild :: Hand -> [Hand]
wild = wild' [[]]
    where
        wild' acc [] = map reverse acc
        wild' acc ('J' : cs) = wild' [z : a | a <- acc, z <- ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']] cs
        wild' acc (c : cs) = wild' (map (c :) acc) cs
 
classify2 :: (Hand, Bid) -> ((Hand, Bid), (HandOrder, CardOrder))
classify2 (h, n) = classify2' $ map (classify . ( \ a -> (a, n))) (wild h)
    where
        classify2' hs = classify2'' $ maximumBy compareHands hs
        classify2'' ((a, b), (c, _)) = ((a, b), (c, cardOrder2 h))

-- Jokers count as 1
cardOrder2 :: Hand -> CardOrder
cardOrder2 h = map ( \ x -> if x == 11 then 1 else x) (cardOrder h)

solve1 :: [(Hand, Bid)] -> Int
solve1 hs = sum $ map ( \ (r, ((_, b), _)) -> r * b) $ zip [1..] $ sortBy compareHands $ map classify hs

solve2 :: [(Hand, Bid)] -> Int
solve2 hs = sum $ map ( \ (r, ((_, b), _)) -> r * b) $ zip [1..] $ sortBy compareHands $ map classify2 hs

day7 :: IO ()
day7 = do
    hands <- parseLines <$> slurpLines "day7.txt"
    let answer1 = solve1 hands
    print $ "part 1: " ++ show answer1
    let answer2 = solve2 hands
    print $ "part 2: " ++ show answer2
 