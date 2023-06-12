module Main (main) where

import Data.Text.IO qualified as T
import Helium.Helium
import Helium.Utility.Compile (AskelleOptions (..), askelleDefaultOptions)
import Helium.Utility.PrettyPrinter

main :: IO ()
main = do
  code <- T.readFile "./heliumTestCases/FailAsIntended/Examples/TypeBug3.hs"
  result <- compileCode "TypeBug3" code askelleDefaultOptions {filterTypeSigs = False}
  case result of
    Left (errTyp, errText) -> do
      print errTyp
      T.putStrLn errText
    Right a -> T.putStrLn $ ppModule a
