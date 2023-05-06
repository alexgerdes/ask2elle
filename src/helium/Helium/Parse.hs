{-# LANGUAGE OverloadedStrings #-}

module Helium.Parse where

import Data.Maybe
import Data.Text qualified as T

import Helium.Main.CompileUtils qualified as Helium

import Helium.Utility.Helium qualified as H
import Language.Haskell.Generated.Syntax qualified as Syn

-- import Language.Haskell.Utility.Utils qualified as Language

type Type = String

-- ! Should we consider using Text instead of String, to bring better performance
parse :: Syn.Name -> Type -> String -> Either String H.Module
parse fnName fnType fnImplementation = H.compile (if isDefined then typeDecl ++ fnImplementation' else fnImplementation') H.defaultOptions
  where
    fnImplementation' :: String 
    fnImplementation' = escape fnImplementation
    fun :: String 
    fun = showName fnName
    isDefined = isJust (H.typeOf fun fnImplementation')
    typeDecl = fun ++ " :: " ++ fnType ++ "\n"


-- | Help functions
-- ! escape converts string and text, back and forth. Can we do everything in Text mode?
escape :: String -> String
escape = inText (\txt -> foldr (\(a, b) txt' -> T.replace a b txt') txt rs)
  where
    rs = [("\\n", "\n"), ("\\\"", "\"")]
    t = T.pack
    inText f = T.unpack . f . t

showName :: Syn.Name -> String
showName n =
    case n of
        Syn.Ident s -> s
        Syn.Operator s -> s
        Syn.Special s -> s