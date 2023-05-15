module Main (main) where

import Data.Text.IO qualified as T
import Helium.Helium
import Helium.Utility.PrettyPrinter

main :: IO ()
main = do
    result <- parse "fromBin" "[Int] -> Int" "fromBin [] = []"
    case result of
        Left (errTyp, errText) -> do
            print errTyp
            T.putStrLn errText
        Right a -> T.putStrLn $ ppModule a
