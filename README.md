# ask2elle
Ask2Elle is an intelligent programming tutor for Haskell

# Weekly concerns
How many files in askelle project were automatically generated?
`Language/Haskell/Syntax.hs` seems to be fully automatically generated from `Language/Haskell/Syntax.ag`

whereas `Language/Haskell/strategy.hs` mixed with generated code and hand-written code.

concerns : should we do a layer of speration? 

maybe this is not possible, as `sem_Expr_List` invokes a function from `Language.Haskell.Rules`, should we at least try to minimize the mix?

