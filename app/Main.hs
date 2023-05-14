module Main (main) where

import Helium.Parse
import Helium.Utility.PrettyPrinter

main :: IO ()
main = do
    result <- parse "fromBin" "[Int] -> Int" "fromBin [] = 1"
    case result of
        Left err -> putStrLn err
        Right a -> putStrLn $ ppModule a