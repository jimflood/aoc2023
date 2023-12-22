module Day17
    ( day17
    ) where

import Lib (slurpLines)
import Data.List (group, groupBy, nub, partition, sort, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Parallel.Strategies (parMap, rseq, rdeepseq)
import Debug.Trace (trace)

type Coordinate = (Int, Int)

type Grid a = Map.Map Coordinate a

type SizedGrid a = ((Int, Int), Grid a)

data Direction = North | South | East | West deriving (Eq, Ord, Show)

type Crucible = (Coordinate, Direction)

type Path = [(Crucible, Int)]

type HeatMap = Map.Map Crucible Int

parseGrid :: [String] -> SizedGrid Int
parseGrid css = ((length (head css), length css) , Map.fromList [((x, y), read [c]) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs])

start :: [Path]
start = [[(((0, 0), East), 0)], [(((0, 0), South), 0)]]

-- end :: SizedGrid -> [Path]
-- end ((mx, my), _) = [[((0, 0), West)], [((0, 0), North)]]

run :: SizedGrid Int -> [Path] -> [Path]
run ((mx, my), g) = step' (Map.fromList (map head start), [], []) -- FIXME start here won't work
    where
        step' :: (HeatMap, [Path], [Path]) -> [Path] -> [Path]
        step' (hm, acc, []) [] = acc
        step' (hm, acc, cache) [] = step' (hm, acc, []) cache
        -- trace (show [length hm, length acc, length cache, length p,length ps]) 
        step' (hm, acc, cache) (p : ps) = trace (show [length hm, length acc, length cache, length p,length ps]) step'' $ filter antiwobble $ filter local $ branch p
            where
                local :: Path -> Bool
                local ((((x, y), d), _) : cs) = x >= 0 && y >= 0 && x < mx && y < my
                local [] = error "???"
                branch :: Path -> [Path]
                branch p = [((measure . advance . left) (head p) : p), ((measure . advance) (head p) : p), ((measure . advance . right) (head p) : p)]
                    where
                        right :: (Crucible, Int) -> (Crucible, Int)
                        right ((c, d), h) = ((c, right' d), h)
                            where
                                right' North = East
                                right' East = South
                                right' South = West
                                right' West = North
                        left :: (Crucible, Int) -> (Crucible, Int)
                        left ((c, d), h) = ((c, left' d), h)
                            where
                                left' North = West
                                left' West = South
                                left' South = East
                                left' East = North
                        advance :: (Crucible, Int) -> (Crucible, Int)
                        advance (((x, y), North), h) = (((x, y - 1), North), h)
                        advance (((x, y), South), h) = (((x, y + 1), South), h)
                        advance (((x, y), East), h) = (((x + 1, y), East), h)
                        advance (((x, y), West), h) = (((x - 1, y), West), h)
                        measure :: (Crucible, Int) -> (Crucible, Int)
                        measure ((pos, dir), h) = ((pos, dir), h + g Map.! pos)
                antiwobble :: Path -> Bool
                antiwobble p = maximum (map length (group (map (snd . fst) p))) <= 3
                --maximum (map length (groupBy ( \ x y -> snd ((fst . fst) x) == snd ((fst . fst) y)) cs)) <= 3
                step'' zs = step''' $ foldl sieve (hm, []) zs
                    where
                        sieve :: (HeatMap, [Path]) -> Path -> (HeatMap, [Path])
                        sieve a b
                            | (fst (head b)) `Map.notMember` (fst a) = (Map.insert (fst (head b)) (snd (head b)) (fst a), b : snd a)
                            | any (\ (c, h) -> h > (fst a) Map.! c) b = a
                            | otherwise = (Map.insert (fst (head b)) (snd (head b)) (fst a), b : snd a)
                            --  if ((fst (head b)) `Map.notMember` (fst a)) || xxx then (Map.insert (fst (head b)) (snd (head b)) (fst a), b : snd a) else a
                            -- where
                            --     xxx = (snd (head b)) < (hm Map.! (fst (head b)))
                step''' :: (HeatMap, [Path]) -> [Path]
                step''' (nhm, nxs) = step'''' $ partition goal nxs
                    where
                        step'''' :: ([Path], [Path]) -> [Path]
                        step'''' (nxs1, nxs2) = step' (nhm, nxs1 ++ acc, nxs2 ++ cache) ps
                        goal :: Path -> Bool
                        goal ((((x, y), _), _) : _) = x == (mx - 1) && y == (my - 1) -- TODO add length check
        step' _ _ = error "Ruh-roh!"

cost :: Path -> Int
cost ((_, h) : _) = h
-- step :: (SizedGrid, HeatMap, [Path], [Path], [Path]) -> [Path]
-- step (sg, hm, acc, cache, ps) p = step' $ neighbors (pos p)
--     where
--         step' = step 

-- neighbors :: Int -> Int -> Coordinate -> [Coordinate]
-- neighbors mx my (x, y) = [(nx, ny) | nx <- [x - 1..x + 1], ny <- [y - 1..y + 1], isLocal (nx, ny), x /= nx || y /= ny]
--     where
--         isLocal (x0, y0) = (x0 <= x0) && (x0 < mx) && (y0 <= y0) && (y0 < my)


-- end :: Grid -> [Cursor]
-- end ((mx, my), g) = [(((mx, my), West, 1), 0, []), (((mx, my), North, 1), 0, [])]

-- right :: Direction -> Direction
-- right North = East
-- right East = South
-- right South = West
-- right West = North

-- left :: Direction -> Direction
-- left North = West
-- left West = South
-- left South = East
-- left East = North

-- solve :: [Cursor] -> Grid -> Maybe (Int, Int) -> [Cursor]
-- -- solve ws ((mx, my), g) u = minimum $ map ( \ (_, b, _) -> b) $ solve' Map.empty ([], []) start
-- solve ws ((mx, my), g) u = solve' Map.empty ([], []) start
--     where
--         solve' :: Map.Map Crucible Int -> ([Cursor], [Cursor]) -> [Cursor] -> [Cursor]
--         solve' _ (a, []) [] = a
--         solve' s (a, b) [] = solve' s (a, []) b
--         -- trace (show [length a, length b, length ps])
--         solve' s (a, b) (p : ps) = solve'' $ catMaybes (parMap rseq (heat . prune . step) (split p)) -- (heat . prune . step)
--             where
--                 goal :: Cursor -> Bool
--                 goal (((x, y), _, n), _, _) =  x + y == 13 -- (x, y) == (mx - 1, my - 1) && n >= maybe 0 fst u -- n + 1 ???
--                 solve'' :: [Cursor] -> [Cursor]
--                 solve'' xs = solve''' (foldl ( \ mm (c, h, _) -> Map.insert c h mm) s xs) (partition goal xs)
--                 solve''' :: Map.Map (Coordinate, Direction, Int) Int -> ([Cursor], [Cursor]) -> [Cursor]
--                 solve''' newS (xs, ys) = solve' newS (a ++ xs, b ++ ys) ps
--                 split (((x, y), d, n), h, t)
--                     | n < maybe (-1) fst u = [(((x, y), d, n), h, t)]
--                     | otherwise = [(((x, y), right d, 0), h, t), (((x, y), left d, 0), h, t), (((x, y), d, n), h, t)]
--                 step :: Cursor -> Cursor
--                 step (((x, y), North, n), h, t) = (((x, y - 1), North, n + 1), h, t)
--                 step (((x, y), South, n), h, t) = (((x, y + 1), South, n + 1), h, t)
--                 step (((x, y), East, n), h, t) = (((x + 1, y), East, n + 1), h, t)
--                 step (((x, y), West, n), h, t) = (((x - 1, y), West, n + 1), h, t)
--                 prune :: Cursor -> Maybe Cursor
--                 prune (((x, y), d, n), h, t)
--                     | n > maybe 3 snd u = Nothing
--                     | (x, y) `Map.notMember` g = Nothing
--                     | any ( \ (((x0, y0), d0, n0), h0) -> s Map.! ((x0, y0), d0, n0) < h0) t = Nothing
--                     | otherwise = Just (((x, y), d, n), h, t)
--                 heat :: Maybe Cursor -> Maybe Cursor
--                 heat (Just (((x, y), d, n), h, t))
--                     | ((x, y), d, n) `Map.notMember` s || s Map.! ((x, y), d, n) > (h + (g Map.! (x, y))) = Just (((x, y), d, n), h + (g Map.! (x, y)), (((x, y), d, n), h) : t)  -- TODO update t elsewhere
--                     | otherwise = Nothing
--                 heat Nothing = Nothing

-- drawPoints :: SizedGrid -> [Coordinate] -> String
-- drawPoints ((mx, my), m) pts = unlines [draw' y | y <- [0..my - 1]]
--     where
--         draw' :: Int -> String
--         draw' y = [if (x, y) `elem` pts then '#' else '.' | x <- [0..mx - 1]]

drawGrid :: SizedGrid a -> Char -> (a -> Char) -> String
drawGrid ((mx, my), m) b f = unlines [drawGrid' y | y <- [0..my - 1]]
    where
        drawGrid' :: Int -> String
        drawGrid' y = []
        drawGrid' y = [maybe b f (Map.lookup (x, y) m) | x <- [0..mx - 1]]

sprite :: Int -> Char
sprite i = head (show i)
-- sprite :: Direction -> Char
-- sprite North = '^'
-- sprite South = 'v'
-- sprite East = '>'
-- sprite West = '<'

day17 :: IO ()
day17 = do
    sg <- parseGrid <$> slurpLines "day17_sample.txt"
    putStr (drawGrid sg 'e' sprite)
    -- print ("grid: " ++ show sg)
    -- putStr (draw grid)
    let xx = run sg start -- solve start grid Nothing
    --print $ "part 1: " ++ show answer1
    -- let x2 = solve (end grid) grid Nothing
    -- print $ "from end " ++ show x2
    -- let ts = [(s, e) | s <- answer1, e <- x2]
    -- print ("length s x e = " ++ show (length ts))
    -- print (show (fst (head ts)))
    -- let cs = filter ( \ (((c1, d1, n1), _, _), ((c2, d2, n2), _, _)) -> c1 == c2 && (d1 == North && d2 == South || d1 == South && d2 == North || d1 == West && d2 == East || d1 == East && d2 == West) && ((n1 + n2) >= 3)) ts -- p1 == p2 && (n1 + n2 >= 3)  ------ ( \ ((p1, n2, _), (p2, n2, _)) -> )
    -- print ("length cs = " ++ show (length cs))
    -- let bs = sort $ map ( \ ((p1, h1, cs1), (p2, h2, cs2)) -> h1 + h2) ts
    -- print ("bs = " ++ show bs)
    --  -- let answer2 = solve start grid (Just (4, 10))
    print $ "part 1: " ++ show xx
    -- let x2 = map (cost sg) answer1
    -- print (show (sort x2))
    let m0 = (fst sg, Map.fromList $ map ( \ c -> ((fst . fst) c, snd c)) (head xx))
    -- putStr (draw m0)
    let x3 = map (fst . fst) (head xx)
    let x4 = sort $ map ( \ x -> snd (head x)) xx
    print (show x4)
    let answer1 = minimum x4
    print $ "part 1: " ++ show answer1
    if answer1 /= 102 then error "wrong answer" else print ":-)"
