module Main (main) where

import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import System.IO 
import Helium.Helium
import Helium.Utility.Compile (AskelleOptions (..), askelleDefaultOptions)
import Helium.Utility.PrettyPrinter
import GhcLib.Compile.Compile (compileToCore)
import GhcLib.Analysis.Analysis (entryPoint)
import Control.Monad.Except (ExceptT, runExceptT, throwError)

-- main :: IO ()
-- main = do
--     code <- T.decodeLatin1 <$> BS.readFile "./heliumTestCases/Success/parser/DerivingMany.hs"
--     result <- compileCode "DerivingMany" code askelleDefaultOptions{filterTypeSigs = False}
--     case result of
--         Left (errTyp, errText) -> do
--             print errTyp
--             T.putStrLn errText
--         Right a -> T.putStrLn $ ppModule a


main :: IO ()
main = do 
  let path = "./ghcTestCases/tasks/Duplicate/shouldMatch/submitSolutions/Duplicate.hs"
  code <- readFile' path 
  entryPoint "Duplicate" code

