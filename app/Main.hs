module Main (main) where

import Helium.Parse
import Helium.Utility.PrettyPrinter
import qualified Data.Text.IO as T 

main :: IO ()
main = do
    result <- parse "fromBin" "[Int] -> Int" "fromBin [] = 1"
    case result of
        Left err -> T.putStrLn err
        Right a -> T.putStrLn $ ppModule a