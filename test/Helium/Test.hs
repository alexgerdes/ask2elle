module Helium.Test (heliumTest) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import Helium.Helium
import System.Directory (listDirectory)
import System.FilePath
import Test.Hspec

-- | Test all the files in the heliumTestCases sub directories
--   The test cases are taken from the Helium project

-- | The subdirectories to test
subdirectories :: [FilePath]
subdirectories =
  fmap
    ("./heliumTestCases/Success/" ++)
    [ "correct/",
      "classesQualified/",
      "benchmarks/",
      "kinderrors/",
      "knowntofix/"
    ]

-- | The main test function
heliumTest :: IO ()
heliumTest = do
  mapM_ go subdirectories
  where
    go :: FilePath -> IO ()
    go filepath = do
      dirs <- fmap (filepath ++) <$> listDirectory filepath
      baseNameAndContent <-
        mapM
          ( \dir -> do
              content <- T.readFile dir
              pure (takeBaseName dir, content)
          )
          dirs
      mapM_ (testFn filepath) baseNameAndContent

    predicate :: (String, T.Text) -> IO String
    predicate (baseName, content) = do
      result <- compileCode (T.pack baseName) content
      case result of
        Left (_errTyp, errText) -> pure $ "Error : " ++ T.unpack errText
        Right _a -> pure "Ok"

    testFn :: FilePath -> (String, T.Text) -> IO ()
    testFn filePath baseNameAndContent@(baseName, _content) = hspec $ describe (filePath ++ baseName) $ do
      it "should compile" $ do
        result <- predicate baseNameAndContent
        result `shouldBe` "Ok"