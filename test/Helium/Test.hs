module Helium.Test (heliumTest) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Helium.Helium
import Helium.Utility.Compile
import System.Directory (listDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Core.Runner

-- | Test all the files in the heliumTestCases sub directories
--   The test cases are taken from the Helium project

-- | The subdirectories to test
-- subdirectories :: [FilePath]
-- subdirectories =
--   fmap
--     ("./heliumTestCases/Success/" ++)
--     [ "correct/",
--       "classesQualified/",
--       "benchmarks/",
--       "kinderrors/",
--       "knowntofix/",
--       "runtimeerrors/"
--     ]

-- | The main test function
heliumTest :: IO ()
heliumTest = do
  successfulCases <- getSubdirectories "./heliumTestCases/Success/"
  mapM_ ("Ok" `typeChecking`) successfulCases
  failedCases <- getSubdirectories "./heliumTestCases/FailAsIntended/"
  mapM_ ("Error" `typeChecking`) failedCases
  where
    typeChecking :: String -> FilePath -> IO ()
    typeChecking expectedOutput filepath = do
      dirs <- fmap (filepath ++) <$> listDirectory filepath
      baseNameAndContent <-
        mapM
          ( \dir -> do
              content <- T.decodeLatin1 <$> BS.readFile dir
              pure (takeBaseName dir, content)
          )
          dirs
      mapM_ (testFn expectedOutput filepath) baseNameAndContent

    predicate :: (String, T.Text) -> IO String
    predicate (baseName, content) = do
      result <- compileCode (T.pack baseName) content $ askelleDefaultOptions {filterTypeSigs = False}
      case result of
        Left (_errTyp, _errText) -> pure "Error"
        Right _a -> pure "Ok"

    testFn :: String -> FilePath -> (String, T.Text) -> IO ()
    testFn expectedOutput filePath baseNameAndContent@(baseName, _content) = hspec $ describe (filePath ++ baseName) $ do
      it "should compile" $ do
        result <- predicate baseNameAndContent
        result `shouldBe` expectedOutput

    getSubdirectories :: FilePath -> IO [FilePath]
    getSubdirectories input = fmap (\subdir -> input </> subdir ++ "/") <$> listDirectory input
