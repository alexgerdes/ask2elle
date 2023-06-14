module Main (main) where

import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Helium.Helium
import Helium.Utility.Compile (AskelleOptions (..), askelleDefaultOptions)
import Helium.Utility.PrettyPrinter

main :: IO ()
main = do
  code <- T.decodeLatin1 <$> BS.readFile "./heliumTestCases/Success/parser/DerivingMany.hs"
  result <- compileCode "DerivingMany" code askelleDefaultOptions {filterTypeSigs = False}
  case result of
    Left (errTyp, errText) -> do
      print errTyp
      T.putStrLn errText
    Right a -> T.putStrLn $ ppModule a


