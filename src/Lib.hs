module Lib
    ( someFunc, slurpLines
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

-- draw :: Grid -> String
-- draw ((mx, my), m) = unlines [draw' y | y <- [0..my - 1]]
--     where
--         draw' :: Int -> String
--         draw' y = [m Map.! (x, y) | x <- [0..mx - 1]]
