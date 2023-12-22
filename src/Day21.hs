module Day21
    ( day21
    ) where

import Lib (slurpLines)
import Data.List (group, groupBy, nub, partition, sort, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Parallel.Strategies (parMap, rseq, rdeepseq)
import Debug.Trace (trace)
import Control.Monad (foldM)


type Coordinate = (Int, Int)

type Grid = Map.Map Coordinate Char

type SizedGrid = ((Int, Int), Grid)

data Direction = North | South | East | West deriving (Eq, Ord, Show)

type Footstep = (Direction, Coordinate)

-- parseGrid :: [String] -> SizedGrid
-- parseGrid css = ((length (head css), length css) , Map.fromList [((x, y), c) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs])

-- drawGrid :: SizedGrid -> Set.Set Coordinate -> String
-- drawGrid ((mx, my), g) s = unlines [drawGrid' y | y <- [0..my - 1]]
--     where
--         drawGrid' :: Int -> String
--         drawGrid' y = [if (x, y) `Set.member` s then 'O' else (g Map.! (x, y)) | x <- [0..mx - 1]]

-- neighbors :: Coordinate -> Set.Set Footstep
-- neighbors (x, y) = Set.fromList [(North, (x, y - 1)), (South, (x, y + 1)), (East, (x + 1, y)), (West, (x - 1, y))]

-- step :: SizedGrid -> (Set.Set Footstep,  Set.Set Coordinate) -> (Set.Set Footstep,  Set.Set Coordinate)
-- step sg (acc, cur) = step' $ foldl ( \ a b -> Set.union a b) Set.empty (map neighbors (Set.toList cur))
--     where
--         step' cs = step'' $ Set.filter ( \ c -> garden sg (snd c)) cs -- (Set.difference cs acc)
--         step'' gs = (Set.union acc gs, Set.map snd gs)

-- garden :: SizedGrid -> Coordinate -> Bool
-- garden ((mx, my), g) (x, y) = g Map.! (x `mod` mx, y `mod` my) /= '#'

-- start :: SizedGrid -> Coordinate
-- start (_, g) = fst $ head $ filter ( \ (k, v) -> v == 'S') (Map.toList g)

-- run :: SizedGrid -> Int -> (Set.Set Footstep,  Set.Set Coordinate)
-- run sg = run' (Set.empty, Set.fromList [start sg])
--     where
--         run' (a, c) 0 = (a, c)
--         run' (a, c) n = run' (step sg (a, c)) (n - 1)

-- pt :: SizedGrid -> Int -> IO String
-- pt sg i = do
--     let (_, c1) = run sg i
--     return (show i ++ "," ++ show (length c1))

parseGrid :: [String] -> SizedGrid
parseGrid css = ((length (head css), length css) , Map.fromList [((x, y), c) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs])

drawGrid :: SizedGrid -> Set.Set Coordinate -> String
drawGrid ((mx, my), g) s = unlines [drawGrid' y | y <- [0..my - 1]]
    where
        drawGrid' :: Int -> String
        drawGrid' y = [if (x, y) `Set.member` s then 'O' else (g Map.! (x, y)) | x <- [0..mx - 1]]

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) = [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]

step :: SizedGrid -> Set.Set Coordinate -> Set.Set Coordinate
step sg cs = Set.fromList $ filter (garden sg) $ nub $ concatMap neighbors (Set.toList cs)

garden :: SizedGrid -> Coordinate -> Bool
garden ((mx, my), g) (x, y) = g Map.! (x `mod` mx, y `mod` my) /= '#'

start :: SizedGrid -> Coordinate
start (_, g) = fst $ head $ filter ( \ (k, v) -> v == 'S') (Map.toList g)

run :: SizedGrid -> Int -> [(Int, Int)]
run sg n = run' [] 0 $ Set.fromList [start sg]
    where
        run' acc i c
            | i == n = acc
            | otherwise = trace (show i) run' ((i, length c) : acc) (i + 1) (step sg c)

day21 :: IO ()
day21 = do
    sg <- parseGrid <$> slurpLines "day21_clear.txt"

    let ts = run sg 200

    let s = map ( \ (a, b) -> show a ++ "," ++ show b) (reverse ts)
    putStr (unlines s)

    print $ "part 1: " ++ show 1
    -- putStr (drawGrid (sz, g) Set.empty)
    -- let (a1, c1) = step g (Set.empty, Set.fromList [(5, 5)])
    -- putStr (drawGrid (sz, g) c1)
    -- print (show [length a1, length c1])
    -- let (a2, c2) = step g (a1, c1)
    -- putStr (drawGrid (sz, g) c2)
    -- print (show [length a2, length c2])
    -- forM [1..10] $ \number -> do
    --     putStr $ show number


    -- let l = [0..100]
    -- let iter acc element = do
    --         let (a1, c1) = run sg element
    --         putStrLn $ show element ++ "," ++ show (length c1)
    --         return (acc + element)
    -- total <- foldM iter 0 l
    -- putStrLn $ "Total is " ++ show total



        -- when (0 == number `mod` 3) $
        --     putStr "Fizz"
        -- when (0 == number `mod` 5) $
        --     putStr "Buzz"
        -- putStrLn (show number)
-- let s0 = do
--         i <- [1..10]
--         s <- pt sg i
--         print s
    -- let (a1, c1) = run sg 100
    -- print (show [100, length c1])
    -- --putStr (drawGrid sg c)
    -- let (a2, c2) = run sg 200
    -- print (show [200, length c2])
    -- --putStr (drawGrid sg c)
    -- let (a3, c3) = run sg 300
    -- print (show [300, length c3])
    --putStr (drawGrid sg c)
 
