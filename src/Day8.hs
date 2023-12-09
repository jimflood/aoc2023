module Day8
    ( day8
    ) where

import Lib (slurpLines)
import Data.List.Split (splitOneOf)
import Data.List ((\\), nub)
import qualified Data.Map as Map

type Instructions = String

type Name = String

type DesertMap = Map.Map Name (Name, Name)

parseLines :: [String] -> (Instructions, DesertMap)
parseLines (x : _ : xs) = (x, Map.fromList (map parse xs))
    where
        parse y = parse' $ filter (not . null) (splitOneOf " =(,)" y)
        parse' [a, b, c] = (a, (b, c))
        parse' _ = error "Huh?"
parseLines _ = error "???"

solve :: DesertMap -> Instructions -> Name -> Int
solve m = solve' 0
    where
        solve' :: Int -> Instructions -> Name -> Int
        solve' acc _ (_ : _ : 'Z' : _) = acc -- happens to only match "ZZZ" starting at "AAA"
        solve' acc ('L' : xs) n = solve' (acc + 1) (xs ++ ['L']) (fst (m Map.! n))
        solve' acc ('R' : xs) n = solve' (acc + 1) (xs ++ ['R']) (snd (m Map.! n))
        solve' _ _ _ = error "???"

solve2 :: DesertMap -> Instructions -> Int
solve2 m i = solve2' $ anodes (Map.keys m)
    where
        -- I found cycles and guessed that this would solve it.
        solve2' ks = solve2'' $map (solve m i) ks
        solve2'' xs = product $ nub $ concatMap ( \ x -> factors x \\ [x]) xs

anodes :: [String] -> [String]
anodes = filter anodes'
    where
        anodes' (_ : _ : 'A' : _) = True
        anodes' _ = False

factors :: Int -> [Int]
factors n = factors' ++map ( \ x -> n `div` x) factors'
    where
        factors' = filter ( \x -> n `mod` x == 0) [1..floor (sqrt (fromIntegral n))]

day8 :: IO ()
day8 = do
    (i, m) <- parseLines <$> slurpLines "day8.txt"
    let answer1 = solve m i "AAA"
    print $ "part 1: " ++ show answer1
    let answer2 = solve2 m i
    print $ "part 2: " ++ show answer2
