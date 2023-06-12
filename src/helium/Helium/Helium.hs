module Helium.Helium (compileFn, compileCode) where

import Data.Text qualified as T
import Helium.Main.CompileUtils qualified as Helium
import Helium.Utility.Compile qualified as Helium

type Type = T.Text

type Name = T.Text

type CodeSnippet = T.Text

type Module = T.Text

compileFn :: Name -> Type -> CodeSnippet -> IO (Either (Helium.HeliumError, T.Text) Helium.Module)
compileFn fnName fnType fnImplementation = do
  -- isDefined <- runExceptT $ Helium.typeOf fnName fnImplementation'
  compilationResult <- Helium.compile False (typeDecl `T.append` fnImplementation') Helium.askelleDefaultOptions
  pure $ fmap Helium.getModule compilationResult
  where
    fnImplementation' :: T.Text
    fnImplementation' = escape fnImplementation
    typeDecl :: T.Text
    typeDecl = fnName `T.append` " :: " `T.append` fnType `T.append` "\n"

compileCode :: Module -> CodeSnippet -> Helium.AskelleOptions -> IO (Either (Helium.HeliumError, T.Text) Helium.Module)
compileCode moduleName code options = do
  compilationResult <- Helium.compile False (escape code) options {Helium.moduleName = Just moduleName}
  pure $ fmap Helium.getModule compilationResult

escape :: T.Text -> T.Text
escape text = foldr (\(a, b) txt' -> T.replace a b txt') text rs
  where
    rs = [("\\n", "\n"), ("\\\"", "\"")]