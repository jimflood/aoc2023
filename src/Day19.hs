module Day19
    ( day19
    ) where

import Lib (slurpLines, tuplify)
import Data.List.Split (splitOn, splitOneOf)
import Data.List (partition)
import qualified Data.Map as Map
import Debug.Trace (trace)

type Category = String

type Part = Map.Map Category Int

type Rule = (Maybe (String, Int -> Bool), String)

type WorkflowMap = Map.Map String [Rule]

type QueueMap = Map.Map String [Part]

type Partition = Map.Map Category [Int]

type PartitionMap = Map.Map String [Partition]

parseLines :: [String] -> (WorkflowMap, [Part])
parseLines = parseLines' . splitSections
    where
        parseLines' [ws, ps] = (Map.fromList (map workflowOf ws), map partOf ps)
        parseLines' _ = error "bad input"
        partOf = Map.map read . Map.fromList . map (tuplify . splitOn "=") . splitOn "," . tail . init
        workflowOf = workflowOf' . tuplify . init . splitOneOf "{}"
            where
                workflowOf' (a, b) = (a, ruleListOf b)
                ruleListOf = map (ruleOf . splitOn ":") . splitOn "," 
                ruleOf [w] = (Nothing, w)
                ruleOf [c, w]
                    | '<' `elem` c = predicateOf (<) $ (tuplify . splitOn "<") c
                    | '>' `elem` c = predicateOf (>) $ (tuplify . splitOn ">") c
                    | otherwise = error "unrecognized operator"
                    where
                        predicateOf :: (Int -> Int -> Bool) -> (String, String) -> Rule
                        predicateOf f (a, b) = (Just (a, \ x -> x `f` read b), w)
                ruleOf _ = error "malformed condition"

splitSections :: [String] -> [[String]]
splitSections = splitSections' [] []
    where
        splitSections' acc g [] = reverse (g : acc)
        splitSections' acc g ("" : xs) = splitSections' (g : acc) [] xs
        splitSections' acc g (x : xs) = splitSections' acc (x : g) xs

initial :: [Part] -> QueueMap
initial ps = Map.fromList [("in", reverse ps)]

run :: WorkflowMap -> QueueMap -> [Part]
run wm qm0 = run' (qm0, [])
    where
        run' :: (QueueMap, [Part]) -> [Part]
        run' (qm, acc) | Map.null qm = acc
        run' (qm, acc) = run' $ foldl runQueue (qm, acc) (Map.keys qm)
             where
                 runQueue (qm', acc') k = runPart (Map.updateLookupWithKey ( \ _ _ -> Nothing) k qm')
                    where
                        runPart (Nothing, _) = error "cannot occur!"
                        runPart (Just ps, qm'') = foldl (runRules (wm Map.! k)) (qm'', acc') ps
                            where
                                runRules :: [Rule] -> (QueueMap, [Part]) -> Part -> (QueueMap, [Part])
                                runRules [] _ _ = error "cannot occur"
                                runRules ((Nothing, w): _) t p = send p w t
                                runRules ((Just (c, f), w) : rs) t p = if f (p Map.! c) then send p w t else runRules rs t p
                                send _ "R" t = t
                                send p "A" (qm''', acc'') = (qm''', p : acc'')
                                send p w (qm''', acc'') = (snd (Map.insertLookupWithKey (\ _ o n -> o ++ n) w [p] qm'''), acc'')

solve :: WorkflowMap -> [Part] -> Int
solve wm ps = solve' $ run wm (initial ps)
    where
        solve' xs = sum $ concatMap Map.elems xs

run2 :: WorkflowMap -> PartitionMap -> [Partition]
run2 wm pm0 = run2' (pm0, [])
    where
        run2' :: (PartitionMap, [Partition]) -> [Partition]
        run2' (pm, acc) | Map.null pm = acc
        run2' (pm, acc) = run2' $ foldl runQueue (pm, acc) (Map.keys pm)
             where
                 runQueue (pm', acc') k = runPart (Map.updateLookupWithKey ( \ _ _ -> Nothing) k pm')
                    where
                        runPart (Nothing, _) = error "cannot occur!"
                        runPart (Just ps, pm'') = foldl (runRules (wm Map.! k)) (pm'', acc') ps
                            where
                                runRules :: [Rule] -> (PartitionMap, [Partition]) -> Partition -> (PartitionMap, [Partition])
                                runRules [] _ _ = error "cannot occur"
                                runRules ((Nothing, w): _) t p = send p w t
                                runRules ((Just (c, f), w) : rs) t p = runRules' (partition f (p Map.! c))
                                    where
                                        runRules' (xs, ys) = runRules rs ( send pleft w t) pright
                                            where
                                                pleft = Map.insert c xs p
                                                pright = Map.insert c ys p
                                send _ "R" t = t
                                send p "A" (pm''', acc'') = (pm''', p : acc'')
                                send p w (pm''', acc'') = (snd (Map.insertLookupWithKey (\ _ o n -> o ++ n) w [p] pm'''), acc'')

--solve2 :: WorkflowMap -> Int
solve2 wm = run2 wm start
    where
        solve2' xs = sum $ concatMap Map.elems xs
        start :: PartitionMap
        start = Map.fromList [("in", [fullPartition])]
        fullPartition :: Partition
        fullPartition = Map.fromList [("x", [1..4000]), ("m", [1..4000]), ("a", [1..4000]), ("s", [1..4000])]

--escribe :: Partition -> [(Int, Int)]
describe p = product $ foldl describe' [] (Map.elems p)
    where
        describe' a b = (maximum b - minimum b + 1) : a

day19 :: IO ()
day19 = do
    (wm, ps) <- parseLines <$> slurpLines "day19.txt"
    let answer1 = solve wm ps
    print $ "part 1: " ++ show answer1
    let answer2 = solve2 wm
    print $ "part 2: " ++ show (sum (map describe answer2))
