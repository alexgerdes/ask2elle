{-# LANGUAGE OverloadedStrings #-}

module Helium.Parse where

import Data.Maybe
import Data.Text qualified as T

import Helium.Main.CompileUtils qualified as Helium

import Helium.Utility.Helium qualified as H
import Language.Haskell.Generated.Syntax qualified as Syn

type Type = String
type Name = String

-- ! Should we consider using Text instead of String, to bring better performance
parse :: Name -> Type -> String -> Either String H.Module
parse fnName fnType fnImplementation = H.compile (if isDefined then typeDecl ++ fnImplementation' else fnImplementation') H.defaultOptions
  where
    fnImplementation' :: String
    fnImplementation' = escape fnImplementation
    isDefined = isJust (H.typeOf fnName fnImplementation')
    typeDecl = fnName ++ " :: " ++ fnType ++ "\n"

-- | Help functions
-- ! escape converts string and text, back and forth. Can we do everything in Text mode?
escape :: String -> String
escape = inText (\txt -> foldr (\(a, b) txt' -> T.replace a b txt') txt rs)
  where
    rs = [("\\n", "\n"), ("\\\"", "\"")]
    t = T.pack
    inText f = T.unpack . f . t
