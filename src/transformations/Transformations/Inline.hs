-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex@botkes.nl
-- Stability   :  provisional
-- Portability :  unknown
--
-----------------------------------------------------------------------------

module Transformations.Inline
   ( inline, removeDeadCode
   ) where

import Data.Generics.Uniplate.Direct
import Data.List (intersect)
import Data.Maybe
import Language.Haskell.Utility.PrettyPrint
import Language.Haskell.Generated.Syntax
import Language.Haskell.Utility.SyntaxInstances
import Transformations.Substitute
import Language.Haskell.Utility.Utils (isRecursive, noShadow, bindings)


-- | inline all but the given pattern bindings
-- a precondition is that all function bindings have been rewritten to pattern
-- bindings
inline :: [Name] -> Module -> Module
inline main_fs m = substitute (mkSubstitution main_fs m) m

mkSubstitution :: [Name] -> Module -> Subst 
mkSubstitution main_fs m
   | not (noShadow m) = error $ "called inline with shadowing: " ++ pprint m
   | otherwise        = foldr update emptySubst substs
   where
      substs = mapMaybe f (universeBi m :: [Decl])
      
      f d@(DPatBind (PVar n) (Rhs e ds))
         | not (isRecursive d) && n `notElem` main_fs = 
           case ds of
             [] -> Just (n, e)
             _  -> Just (n, Let ds e)
      f _ = Nothing
      
      update (n, e) ctx = let e' = substitute ctx e in insertSubst n e' ctx

removeDeadCode :: [Name] -> Module -> Module
removeDeadCode main_fs m = 
    repairLets $ transformBi f $ transformBi g $ transformBi h m
  where
    f (Body ds)     = Body $ filter p ds
    f x             = x
    g (Let ds e)    = Let (filter p ds) e
    g x             = x
    h (Rhs e ds)    = Rhs e $ filter p ds
    h x             = x 
    
    p (DPatBind (PVar n) _) = n `elem` ns
    p _ = True
    
    ns = functionsInUse m ++ main_fs

-- | Only valid after alpha renamaning (no shadowing)
functionsInUse :: BiplateFor a => a -> [Name]
functionsInUse x 
   | noShadow x = bindings x `intersect` [name | Var name <- universeBi x]
   | otherwise  = error "Utils.hs - functionsInUse: called with shadowing!"

repairLets :: Module -> Module
repairLets = transformBi f
   where
      f (Let [] e) = e
      f e = e
