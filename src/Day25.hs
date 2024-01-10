module Day25
    ( day25
    ) where

import Lib (slurpLines)
import Data.List (groupBy, partition, sortOn)
import Data.List.Split (splitOneOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Parallel.Strategies (parMap, rdeepseq)

type Node = String

type Graph = Map.Map Node (Set.Set Node)

parseLines :: [String] -> Graph
parseLines = graphOf . concatMap (parse . splitOneOf ": ")
    where
        parse (x : _ : xs) = foldl ( \ a b -> (x, b) : (b, x) : a) [] xs
        parse _ = error "Ruh-roh"
        graphOf xs = mapify $ groupBy ( \ a b -> fst a == fst b) (sortOn fst xs)
        mapify = Map.fromList . map ( \ ts -> (fst (head ts), Set.fromList (map snd ts)))

paths :: Graph -> Node -> Node -> Int
paths m r k = paths' [] Set.empty [] [(k, Set.empty)]
    where
        paths' :: [Set.Set Node] -> Set.Set Node -> [(Node, Set.Set Node)] -> [(Node, Set.Set Node)] -> Int
        paths' acc _ [] [] = length acc
        paths' acc g ys [] = paths' acc g [] ys
        paths' acc g ys ((n, s) : ns)
            | length acc > 3 = length acc -- if more than 3 we've seen enough for this one
            | (not . Set.null . Set.intersection g) s = paths' acc g ys ns
            | n == r = paths' (intra : acc) (Set.union intra g) ys ns
            | otherwise = paths' acc g (foldl sieve ys (m Map.! n)) ns
                where
                    intra = Set.delete k s
                    sieve :: [(Node, Set.Set Node)] -> Node -> [(Node, Set.Set Node)]
                    sieve a b
                        | b == k = a
                        | b `Set.member` s = a
                        | b `Set.member` g = a
                        | any (( \ x -> b `Set.member` x) . snd) ys = a
                        | otherwise = (b, Set.insert n s) : a

solve :: Graph -> Int
solve m = calculate $ partition (> 3) $ solve' (Map.keys m)
    where
        solve' (r : ns) = parMap rdeepseq (paths m r) ns
        solve' _ = error "???"
        calculate (a, b) = (length a + 1) * length b

day25 :: IO ()
day25 = do
    m <- parseLines <$> slurpLines "day25.txt"
    let answer1 = solve m
    print $ "part 1: " ++ show answer1
