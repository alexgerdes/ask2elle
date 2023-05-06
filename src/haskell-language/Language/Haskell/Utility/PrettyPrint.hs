{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Maintainer  :  alex@botkes.nl
-- Stability   :  provisional
-- Portability :  unknown
module Language.Haskell.Utility.PrettyPrint
    ( -- * Pretty printing
      PrettyPrint
    , pprint
    ) where

import Data.Generics.Uniplate.Direct
import qualified Helium.Utility.Helium as H
import Ideas.Common.Library
import Language.Haskell.Generated.Syntax
import Language.Haskell.Utility.Views

-- | Pretty print class
class PrettyPrint a where
    pprint :: a -> String

instance PrettyPrint Module where
    pprint = H.ppModule . build heliumView . clean
instance PrettyPrint Decl where
    pprint = H.ppDeclaration . build heliumView . clean
instance PrettyPrint Expr where
    pprint = H.ppExpression . build heliumView
instance PrettyPrint (Context Module) where
    pprint = maybe "no module from context" pprint . fromContext

clean :: Biplate a Rhs => a -> a
clean = transformBi f
  where
    f (Rhs e [DHole _]) = Rhs e []
    f (GRhs e [DHole _]) = GRhs e []
    f x = x
