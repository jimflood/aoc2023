module Lib
    ( someFunc, slurpLines
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename
