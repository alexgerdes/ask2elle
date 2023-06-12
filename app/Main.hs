module Main (main) where

import Data.Text.IO qualified as T
import Helium.Helium
import Helium.Utility.PrettyPrinter

main :: IO ()
main = do
    code <- T.readFile "./heliumTestCases/Success/correct/Eq.hs"
    result <- compileCode "Eq1" code
    case result of
        Left (errTyp, errText) -> do
            print errTyp
            T.putStrLn errText
        Right a -> T.putStrLn $ ppModule a
