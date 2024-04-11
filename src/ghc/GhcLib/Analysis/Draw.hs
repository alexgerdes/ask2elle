module GhcLib.Analysis.Draw where

import Text.Tabular

import Data.List (transpose)
import GhcLib.Analysis.TestHoleMapping
import GhcLib.Transform.Transform

example :: Int -> [TableColumn] -> Table String String String
example testCount columns =
    let row = transpose $ map (\(TableColumn x y z) -> x ++ y ++ z) columns
    in  Table
            ( Group
                SingleLine
                [ Group NoLine $ fmap (Header . show) allNormalizationOptions
                , Group NoLine $ fmap (Header . show) allPostNormalizationOptions
                , Group NoLine $
                    fmap Header ["comparisonCount", "matchedCount", "unmatchedCount", "ratio"]
                ]
            )
            ( Group
                SingleLine
                [ Group SingleLine $ fmap (Header . show) [1 .. testCount]
                ]
            )
            row
