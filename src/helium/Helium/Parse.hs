{-# LANGUAGE OverloadedStrings #-}

module Helium.Parse where

import Data.Maybe
import Data.Text qualified as T

import Helium.Main.CompileUtils qualified as Helium

import Control.Monad.Except (runExceptT)
import Data.Either (isLeft)
import Helium.Utility.Helium qualified as H
import Language.Haskell.Generated.Syntax qualified as Syn

type Type = T.Text
type Name = T.Text
type CodeSnippet = T.Text

-- ! Run compile two times
parse :: Name -> Type -> CodeSnippet -> IO (Either T.Text Helium.Module)
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
