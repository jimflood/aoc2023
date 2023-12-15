module Day12
    ( day12
    ) where

import Data.List.Split (splitOn)
import Data.List (sortOn)
import Lib (slurpLines)
import qualified Data.Map as Map
import Control.Parallel.Strategies (parMap, rdeepseq)

type Row = (String, [Int])

type CountKey = (Int, Int)

type CountMap = Map.Map CountKey Int

data RowCursor = RC (Map.Map Int Char) [Int] Int Int Int [CountKey] deriving Show

parseLines :: [String] -> [Row]
parseLines = map (parse . splitOn " ")
    where
        parse (xs : ys : _) = (xs, map ( \ x -> read x::Int) (splitOn "," ys))
        parse _ = error "???"

unfold :: [Row] -> [Row]
unfold = map unfold'
    where
        unfold' (r, ns) = (r ++ "?" ++ r ++ "?" ++ r ++ "?" ++ r ++ "?" ++ r, ns ++ ns ++ ns ++ ns ++ ns)

-- This travels down the string, keeping state, and splitting into multiple parsers when the path diverges.
cursor :: Row -> RowCursor
cursor (cs, ns) = RC (store cs) ns (sum ns + length ns - 1) (length cs) 0 [(0, 0)]

-- for speed of access to string
store :: String -> Map.Map Int Char
store xs = Map.fromList $ zip [0..] xs

-- I don't really need parMap in the end. With it, 5 seconds, without it, 8 seconds.
solve :: [Row] -> Int
solve rs = sum $ parMap rdeepseq (arrange . cursor) rs

-- Any cursor that makes it to the end has accumulated the counts of all cursors that joined it.
total :: CountMap -> [RowCursor] -> Int
total cm rcs = sum $ map total' rcs
    where
        total' (RC _ _ _ _ _ (k : _)) = cm Map.! k
        total' _ = error "Cannot occur"

-- state machine that works like a regular expression
arrange :: RowCursor -> Int
arrange x = arrange' [] (Map.singleton (0, 0) 1, [('*', x)])
    where
        arrange' :: [RowCursor] -> (CountMap, [(Char, RowCursor)]) -> Int
        arrange' acc (cm, []) = total cm acc
        arrange' acc (cm, ('*', rc) : rcs) = arrange' acc (priority (asterisk (cm, rcs) rc))
        arrange' acc (cm, ('#', rc) : rcs) = arrange' acc (priority (spring (cm, rcs) rc))
        arrange' acc (cm, ('.', rc) : rcs) = arrange' acc (priority (spacer (cm, rcs) rc))
        arrange' acc (cm, ('$', rc) : rcs) = arrange' acc (priority (dollar (cm, rcs) rc))
        arrange' acc (cm, ('+', rc) : rcs) = arrange' (rc : acc) (cm, rcs)
        arrange' a b = error ("Ruh-roh: " ++ show a ++ " " ++ show b)

-- keep them syncronized between springs when cursors meet up
priority :: (CountMap, [(Char, RowCursor)]) -> (CountMap, [(Char, RowCursor)])
priority (cm, rcs) = (cm, sortOn sk rcs)
    where
        sk :: (Char, RowCursor) -> Int
        sk (_, (RC _ _ _ _ i _)) = i

-- match zero or more '.'
asterisk :: (CountMap, [(Char, RowCursor)]) -> RowCursor -> (CountMap, [(Char, RowCursor)])
asterisk (cm, rcs) (RC m ns rsvd len i ks) = (cm, foldl asterisk' rcs [0..(limit 0)])
    where
        limit d
            | d == len - i - rsvd = d
            | (i + d) `Map.notMember` m = d
            | m Map.! (i + d) == '#' = d
            | otherwise = limit (d + 1)
        asterisk' :: [(Char, RowCursor)] -> Int -> [(Char, RowCursor)]
        asterisk' a b = ('#', RC m ns rsvd len (i + b) ks) : a

-- match spring, e.g. ###
-- The key (length ns, i) is the point where cursors join and only one continues, aggregating
-- counts. Joining paths took the runtime from 17 hours and still not finished, to 5 seconds.
spring :: (CountMap, [(Char, RowCursor)]) -> RowCursor -> (CountMap, [(Char, RowCursor)])
spring (cm, rcs) (RC m ns rsvd len i ks)
    | noDots = spring' $ Map.insertLookupWithKey ( \ _ nv ov -> nv + ov) (length ns, i) (cm Map.! (head ks)) cm
    | otherwise = (cm, rcs)
        where
            spring' (Nothing, ncm) = (ncm, (nextState, RC m (tail ns) (max 0 (rsvd - head ns - 1)) len (i + head ns) ((length ns, i) : ks)) : rcs)
            spring' (Just _, ncm) = (ncm, rcs)
            noDots = all ( \ k -> (k `Map.member` m) && m Map.! k /= '.') [i..i + head ns - 1]
            nextState = if length ns == 1 then '$' else '.'

-- match exactly 1 '.'
spacer :: (CountMap, [(Char, RowCursor)]) -> RowCursor -> (CountMap, [(Char, RowCursor)])
spacer (cm, rcs) (RC m ns rsvd len i ks)
    | m Map.! i /= '#' = (cm, ('*', RC m ns rsvd len (i + 1) ks) : rcs)
    | otherwise = (cm, rcs)

-- match zero or more '.' up to the end of the string
dollar :: (CountMap, [(Char, RowCursor)]) -> RowCursor -> (CountMap, [(Char, RowCursor)])
dollar (cm, rcs) (RC m ns rsvd len i ks)
    | noHashes = (cm, ('+', RC m ns rsvd len i ks) : rcs)
    | otherwise = (cm, rcs)
        where
            noHashes = all ( \ k -> (k `Map.member` m) && m Map.! k /= '#') [i..len - 1]

day12 :: IO ()
day12 = do
    input <- parseLines <$> slurpLines "day12.txt"
    let answer1 = solve input
    print $ "part 1: " ++ show answer1
    let answer2 = solve (unfold input)
    print $ "part 2: " ++ show answer2
