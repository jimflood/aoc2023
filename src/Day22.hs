module Day22
    ( day22
    ) where

import Lib (slurpLines)
import Data.List (groupBy, partition, sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOneOf)
import qualified Data.Matrix as Matrix -- An idea that I did not use in the end.

type Cube = (Int, Int, Int)

type Brick = Set.Set Cube

type Graph = Map.Map Brick (Set.Set Brick)

parseLines :: [String] -> [Brick]
parseLines = map (parse . map ( \x -> read x :: Int) . splitOneOf ",~")
    where
        parse [xa, ya, za, xb, yb, zb] = brickFrom $ Matrix.fromList 3 2 [xa, xb, ya, yb, za, zb]
        parse _ = error "bad input"
        brickFrom mat = cubesetOf (Matrix.toLists mat)
            where
                cubesetOf [[x1, x2], [y1, y2], [z1, z2]] = Set.fromList [(x, y, z) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2], z <- [min z1 z2..max z1 z2]]
                cubesetOf _ = error "???"

conflicts :: Brick -> Brick -> Bool
conflicts a b = (not . Set.null $ Set.intersection a b)

drop1z :: Brick -> Brick
drop1z b = Set.map ( \ (x, y, z) -> (x, y, z - 1)) b

supports :: Brick -> Brick -> Bool
supports b a = (a /= b) && (conflicts a (drop1z b)) && (not (conflicts a b))

bottom :: Brick -> Int
bottom b = minimum (Set.map ( \ (_, _, z) -> z) b)

settle :: [Brick] -> [Brick]
settle xs = settle' ([], sortOn bottom xs)
    where
        settle' :: ([Brick], [Brick]) -> [Brick]
        settle' (acc, []) = acc
        settle' (acc, (b : bs))
            | (any (conflicts b) acc) = error "unexpected collision"
            | bottom b == 1 = settle' ((b : acc), bs)
            | (any (conflicts (drop1z b)) acc) = settle' ((b : acc), bs)
            | otherwise = settle' (acc, (drop1z b) : bs)

graph :: [Brick] -> Graph
graph bs = foldl graph' Map.empty bs
    where
        graph' m b = Map.insert b (Set.fromList (filter (\ x -> supports x b) bs)) m

invert :: Graph -> Graph
invert g = Map.fromList $ map ( \ xs -> (fst (head xs), Set.fromList (map snd xs))) $ groupBy ( \ a b -> fst a == fst b) $ sortOn fst [(m, i) | i <- Map.keys g, m <- Set.toList (g Map.! i)]

-- -- % dot -Tpng -o 22.png 22.dot && open 22.png
-- digraph :: Graph -> [Brick] -> [Brick] -> [String]
-- digraph m bs s = "digraph g {" : "node [style=filled];" : ((map node bs) ++ (foldl edges ["}"] bs))
--     where
--         edges :: [String] -> Brick -> [String]
--         edges acc k = (foldl edges' acc (Map.findWithDefault Set.empty k m))
--             where
--                 edges' :: [String] -> Brick -> [String]
--                 edges' a b = (label k ++ " -> " ++ label b ++ ";") : a
--         node n = label n ++ " [label=\"" ++ (drop 9 (show n)) ++ "\"" ++ color ++ "];"
--             where
--                 color
--                     | (n `elem` s) && (bottom n == 1) = ",color=red"
--                     | (n `elem` s) = ",color=yellow"
--                     | bottom n == 1 = ",color=saddlebrown"
--                     | otherwise = ",color=lightgray"
--         label :: Brick -> String
--         label x = reverse $ foldl tr [] (show (Set.toList x))
--         tr :: String -> Char -> String
--         tr acc c
--             | isDigit c = c : acc
--             | otherwise = '_' : acc

candidates :: Graph -> Graph -> [Brick] -> [Brick]
candidates g ig bs = filter candidates' bs
    where
        candidates' k = all ( \ x -> (length (ig Map.! x)) /= 1) (g Map.! k)

count :: Graph ->[Brick] -> Brick -> Int
count ig bs b = count' (Set.singleton b) (filter (/= b) bs)
    where
        count' xs ys = count'' $ partition ( \ y -> (y `Map.member` ig) && ((ig Map.! y) `Set.isSubsetOf ` xs)) ys
            where
                count'' ([], _) = length xs - 1
                count'' (nxs, nys) = count' (Set.union xs (Set.fromList nxs)) (nys)

day22 :: IO ()
day22 = do
    input <- parseLines <$> slurpLines "day22.txt"
    let bricks = settle input
    let g = graph bricks
    let ig = invert g
    let ds = candidates g ig bricks
    -- putStrLn (unlines (digraph ig bricks ds))
    let answer1 = length ds
    print $ "part 1: " ++ show answer1
    let answer2 = sum $ map (count ig bricks) bricks
    print $ "part 2: " ++ show answer2
