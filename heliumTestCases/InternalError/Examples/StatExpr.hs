{-
Correct output copied from Helium Project 

Prelude is up to date
Compiling typeerrors/Examples/StatExpr.hs
(3,5): Type error in overloaded do-expression
 expression       : do "koe"
   type           : String
 problem          : [] is not an instance of class Monad
 hint             : valid instances of Monad are IO and Maybe

Compilation failed with 1 error

The error we get 

INTERNAL ERROR - illegal type
** Module   : Top.Types.Unification
** Function : mguWithTypeSynonyms

CallStack (from HasCallStack):
  error, called at src/Utils.hs:13:4 in Top-1.9-inplace:Utils


-}
module StatExpr where

f = do "koe"
