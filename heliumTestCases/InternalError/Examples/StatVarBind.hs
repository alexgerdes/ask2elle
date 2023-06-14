{-
Output fetched from helium project

Prelude is up to date
Compiling typeerrors/Examples/StatVarBind.hs
(4,27): Type error in else branch of conditional
 expression       : if x then m else x
 term             : x
   type           : Bool   
   does not match : IO Bool

Compilation failed with 1 error

The error we get 

INTERNAL ERROR - illegal type
** Module   : Top.Types.Unification
** Function : mguWithTypeSynonyms

CallStack (from HasCallStack):
  error, called at src/Utils.hs:13:4 in Top-1.9-inplace:Utils
-}
module StatVarBind where

f m = do x <- m
         if x 
          then m 
          else x
