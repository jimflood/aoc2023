module Day20
    ( day20
    ) where

import Lib (slurpLines, tuplify)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as Map
import Data.Char (isAlpha, isSpace, ord)
import Data.List (findIndex)
import Data.List (groupBy, sortOn)

type Graph = Map.Map String [String]

type InvertedGraph = Map.Map String [String]

type Pulse = Bool -- true = high pulse, false = low pulse

data Module = Module { typ :: Char 
                     , inputs :: Map.Map String Pulse
                     , outputs :: [String] } deriving (Eq, Ord, Show)

type ModuleMap = Map.Map String Module

data Message = Message { from :: String
                       , pulse :: Bool
                       , to :: String } deriving Show

type Mailbox = [Message]

type Counter = (Int, Int, Int)

parseLines :: [String] -> (Graph, [String])
parseLines = processKeys . parseGraph
    where
        parseGraph = foldl parseGraph' Map.empty
        parseGraph' :: Graph -> String -> Graph
        parseGraph' m cs = parseGraph'' $ splitOn ">" (tr cs)
            where
                tr s = [c | c <- s, c `notElem` " -"]
                parseGraph'' [h, ts] = foldl ( \ a b -> Map.insertWithKey ( \ k o n -> n ++ o) h [b] a) m (splitOn "," ts)
                parseGraph'' _ = error "bad input"
        processKeys m = (Map.mapKeys (last . splitOneOf "%&") m, Map.keys m)

invert :: Graph -> InvertedGraph
invert g = Map.fromList $ map ( \ xs -> (fst (head xs), map snd xs)) $ groupBy ( \ a b -> fst a == fst b) $ sortOn fst [(m, i) | i <- Map.keys g, m <- g Map.! i]

moduleMap :: Graph -> InvertedGraph -> [String] -> ModuleMap
moduleMap g ig = addOutputs . Map.fromList . moduleMap' []
    where
        moduleMap' acc [] = acc
        moduleMap' acc (('%' : t) : xs) = moduleMap' ((t, Module '%' (Map.singleton "*" False) (g Map.! t)) : acc) xs
        moduleMap' acc (('&' : t) : xs) = moduleMap' ((t, Module '&' (Map.fromList [(k, False) | k <- ig Map.! t]) (g Map.! t)) : acc) xs
        moduleMap' acc ("broadcaster" : xs) = moduleMap' (("broadcaster", Module 'B' Map.empty (g Map.! "broadcaster")) : acc) xs
        moduleMap' _ _ = error "Ruh-roh"
        addOutputs :: ModuleMap -> ModuleMap
        addOutputs mm = foldl addOutputs' mm (Map.keys ig)
            where
                addOutputs' :: ModuleMap -> String -> ModuleMap
                addOutputs' a b
                    | b `Map.member` a = a
                    | otherwise = Map.insert b (Module '$' (Map.singleton "$" False) []) a

run :: (ModuleMap, Counter) -> Mailbox -> (ModuleMap, Counter)
run mc mbox = run' $ foldl deliver (mc, []) mbox
    where
        run' (mc', []) = mc'
        run' (mc', mbox') = run mc' (reverse mbox')

deliver :: ((ModuleMap, Counter), Mailbox) -> Message -> ((ModuleMap, Counter), Mailbox)
deliver ((mm, (a, b, c)), mbox) msg = (increment . deliver') (mm Map.! (to msg))
    where
        deliver' :: Module -> (ModuleMap, Mailbox)
        deliver' m
            | typ m == '%' = flipFlop (mm, mbox) m msg
            | typ m == '&' = conjunction (mm, mbox) m msg
            | typ m == 'B' = broadcaster (mm, mbox) m msg
            | typ m == '$' = output (mm, mbox) m msg
            | otherwise = error "Nope"
        increment (newMm, newMbox) = ((newMm, newC (pulse msg)), newMbox)
        newC True = (a + 1, b, c)
        newC False
            | to msg == "qt" = (a, b + 1, c + 1)
            | otherwise = (a, b + 1, c)

flipFlop :: (ModuleMap, Mailbox) -> Module -> Message -> (ModuleMap, Mailbox)
flipFlop (mm, mbox) m (Message _ p t)
    | not p = flipFlop' (Module (typ m) (Map.adjust not "*" (inputs m)) (outputs m))
    | otherwise = (mm, mbox)
    where
        flipFlop' newM = (Map.insert t newM mm, foldl ( \ a b -> Message t signal b: a) mbox (outputs newM))
            where
                signal = inputs newM Map.! "*"

conjunction :: (ModuleMap, Mailbox) -> Module -> Message -> (ModuleMap, Mailbox)
conjunction (mm, mbox) m (Message f p t) = conjunction' (Module (typ m) (Map.insert f p (inputs m)) (outputs m))
    where
        conjunction' :: Module -> (ModuleMap, Mailbox)
        conjunction' newM = (Map.insert t newM mm, foldl ( \ a b -> Message t signal b : a) mbox (outputs newM))
            where
                signal
                    | all id (Map.elems (inputs newM)) = False
                    | otherwise = True

broadcaster :: (ModuleMap, Mailbox) -> Module -> Message -> (ModuleMap, Mailbox)
broadcaster (mm, mbox) m (Message _ p t) =  (mm, foldl ( \ a b -> Message t p b : a) mbox (outputs m))

output :: (ModuleMap, Mailbox) -> Module -> Message -> (ModuleMap, Mailbox)
output (mm, mbox) m (Message _ p t) = output' (Module (typ m) (Map.insert "$" p (inputs m)) (outputs m))
    where
        output' newM = (Map.insert t newM mm, mbox)

pressButton :: (ModuleMap, Counter) -> (ModuleMap, Counter)
pressButton mc = run mc broadcast
    where
        broadcast :: [Message]
        broadcast = [Message "button" False "broadcaster"]

solve1 :: ModuleMap -> (ModuleMap, Counter)
solve1 mm = foldl ( \ a _ -> pressButton a) (mm, (0, 0, 0)) [1..1000] 

solve2 :: ModuleMap -> (Int, (ModuleMap, Counter))
solve2 mm = solve2' 1 $ pressButton (mm, (0, 0, 0))
    where
        solve2' acc (newMm, (a, b, c))
            | c < 1 = solve2' (acc + 1) $ pressButton (newMm, (a, b, c))
            | otherwise = (acc, (newMm, (a, b, c)))

digraph :: ModuleMap -> String
digraph mm = digraph' "}\n" $ Map.keys mm
    where
        digraph' acc [] = "digraph g {\n" ++ acc
        digraph' acc (k : ks) = digraph' (foldl ( \ a  b -> k ++ " [label=\"" ++ label ++ "\",color=" ++ color ++ "];\n" ++ k ++ " -> " ++ b ++ ";\n" ++ a) acc (outputs (mm Map.! k))) ks
            where
                label
                    | typ (mm Map.! k) == '%' = k ++ "\\n" ++ "FLIP-FLOP"
                    | typ (mm Map.! k) == '&' = k ++ "\\n" ++ "CONJUNCTOR"
                    | k == "rx" = k ++ "\\n" ++ "GOAL"
                    | otherwise = k
                color
                    | typ (mm Map.! k) == '%' = "yellow"
                    | typ (mm Map.! k) == '&' = "orange"
                    | k == "rx" = "lightgreen"
                    | otherwise = "lightgray"
    
day20 :: IO ()
day20 = do
    input <- parseLines <$> slurpLines "day20.txt"
    let (g, ks) = input
    let ig = invert g
    let mm = moduleMap g ig ks

    putStr (digraph mm)


    let (_, (a, b, c)) = solve1 mm
    print (show (a, b, c))
    print (show (a * b))

    let (i, (mm2, c2)) = solve2 mm
    print ("first " ++ show i)

    let (i2, (mm3, c3)) = solve2 mm2
    print ("second " ++ show i)

    -- print $ "part 2: " ++ show answer2
