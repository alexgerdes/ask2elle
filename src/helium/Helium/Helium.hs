{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Helium.Helium where

import Data.Maybe
import qualified Data.Text as T

import qualified Helium.Main.CompileUtils as Helium

import Control.Monad.Except (runExceptT)
import Data.Either (isLeft)
import qualified Helium.Utility.Compile as H
import qualified Language.Haskell.Generated.Syntax as Syn

type Type = T.Text
type Name = T.Text
type CodeSnippet = T.Text

-- ! Run compile two times
parse :: Name -> Type -> CodeSnippet -> IO (Either (H.HeliumError, T.Text) Helium.Module)
parse fnName fnType fnImplementation = do
    -- isDefined <- runExceptT $ H.typeOf fnName fnImplementation'
    compilationResult <- H.compile False (typeDecl `T.append` fnImplementation') H.askelleDefaultOptions{H.filterTypeSigs = False}
    pure $ fmap H.getModule compilationResult
  where
    fnImplementation' :: T.Text
    fnImplementation' = escape fnImplementation
    typeDecl = fnName `T.append` " :: " `T.append` fnType `T.append` "\n"

-- | Help functions
-- ! escape converts string and text, back and forth. Can we do everything in Text mode?
escape :: T.Text -> T.Text
escape = inText (\txt -> foldr (\(a, b) txt' -> T.replace a b txt') txt rs)
  where
    rs = [("\\n", "\n"), ("\\\"", "\"")]
    t = T.pack
    inText f = f
