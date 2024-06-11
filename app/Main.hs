module Main (main) where

-- import Helium.Helium
-- import Helium.Utility.Compile (AskelleOptions (..), askelleDefaultOptions)
-- import Helium.Utility.PrettyPrinter

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.ByteString qualified as BS
import Data.List (permutations, sortBy, subsequences)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import GhcLib.Analysis.Analysis
import GhcLib.Analysis.Draw
import GhcLib.Analysis.TestHoleMapping
import GhcLib.Compile.Compile
import GhcLib.Transform.Transform
import System.IO
import Text.Tabular.Csv

-- >>> allCombinations [1,2,3]

-- main :: IO ()
-- main = do
--     code <- T.decodeLatin1 <$> BS.readFile "./heliumTestCases/Success/parser/DerivingMany.hs"
--     result <- compileCode "DerivingMany" code askelleDefaultOptions{filterTypeSigs = False}
--     case result of
--         Left (errTyp, errText) -> do
--             print errTyp
--             T.putStrLn errText
--         Right a -> T.putStrLn $ ppModule a

-- printOutTargetCore :: IO ()
-- printOutTargetCore = do
--     let studentSolP= "./ghcTestCases/tasks/Duplicate/shouldMatch/submitSolutions/Test2.hs"
--     stdSolCode <- readFile' studentSolP
--     let modelSolP= "./ghcTestCases/tasks/Duplicate/modelSolutions/Mod3.hs"
--     modelSolCode <- readFile' modelSolP
--     result <- comparePrograms compDesugar True ("Test2",stdSolCode) ("Mod3",modelSolCode)
--     print result

main :: IO ()
main = do
  -- summarizeComparisonResult result
  let allOptions =
        [ (x, y) | x <- sortBy (\x y -> compare (length x) (length y)) $ allPossibleNormalizationOptions, y <- [allPostNormalizationOptions]
        ]
  testCount <-
    mapM
      ( \(normalChoice, postNormalChoice) -> do
          putStrLn $
            "Performing analysis with normalization options: "
              ++ show normalChoice
              ++ " with post choice :"
              ++ show postNormalChoice
          calculateRatio normalChoice postNormalChoice
      )
      [head allOptions]
  let table = render id id id $ example (length testCount) testCount
  writeFile "./analysingResult.csv" table
