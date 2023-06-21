module Helium.Test (heliumTest) where

import Conduit
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory (listDirectory)
import System.FilePath
import Test.Hspec
import Test.Hspec.Core.Runner

import Helium.Helium
import Helium.Utility.Compile

-- | Test all the files in the heliumTestCases sub directories
--   The test cases are taken from the Helium project
-- test :: IO ()
-- test =

-- | The main test function
heliumTest :: IO ()
heliumTest = do
    runConduitRes $ sourceDirectoryDeep False "./heliumTestCases/Success/" .| filterC (isExtensionOf ".hs") .| mapM_C (lift . typeChecking "Ok")
    runConduitRes $ sourceDirectoryDeep False "./heliumTestCases/FailAsIntended/" .| filterC (isExtensionOf ".hs") .| mapM_C (lift . typeChecking "Error")
  where
    typeChecking :: String -> FilePath -> IO ()
    typeChecking expectedOutput filepath = do
        content <- T.decodeLatin1 <$> BS.readFile filepath
        testFn expectedOutput filepath (takeBaseName filepath, content)

    predicate :: (String, T.Text) -> IO String
    predicate (baseName, content) = do
        result <- compileCode (T.pack baseName) content $ askelleDefaultOptions{filterTypeSigs = False}
        case result of
            Left (_errTyp, _errText) -> pure "Error"
            Right _a -> pure "Ok"

    testFn :: String -> FilePath -> (String, T.Text) -> IO ()
    testFn expectedOutput filePath baseNameAndContent@(baseName, _content) = hspec $ describe (filePath ++ baseName) $ do
        it "should compile" $ do
            result <- predicate baseNameAndContent
            result `shouldBe` expectedOutput
