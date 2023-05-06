
# Weekly concerns 
How many files in askelle project were automatically generated?
`Language/Haskell/Syntax.hs` seems to be fully automatically generated from `Language/Haskell/Syntax.ag`

whereas `Language/Haskell/strategy.hs` mixed with generated code and hand-written code.

concerns : should we do a layer of speration? 

maybe this is not possible, as `sem_Expr_List` invokes a function from `Language.Haskell.Rules`, should we at least try to minimize the mix?

Answered :

Strategy is still automatically generated, since `uuagc` can take haskell code as input. `uuagc` only do generation on the left side of $=$ sign, it merely copies the code on the right hand  to the generated `hs` file. That's why we see function from `Language.Haskell.Rules` module appear in the `sem_Expr_List `. 
To modify `strategy.hs`, the recommand approach is to modify `strategy.ag` first, then generate `strategy.hs`. 
`uuagc` can be found via `cabal list` and install with `cabal install`