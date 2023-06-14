
# Weekly concerns 
1. How many files in askelle project were automatically generated?
  
`Language/Haskell/Syntax.hs` seems to be fully automatically generated from `Language/Haskell/Syntax.ag`

whereas `Language/Haskell/strategy.hs` mixed with generated code and hand-written code.

concerns : should we do a layer of speration? 

maybe this is not possible, as `sem_Expr_List` invokes a function from `Language.Haskell.Rules`, should we at least try to minimize the mix?

Answered :

Strategy is still automatically generated, since `uuagc` can take haskell code as input. `uuagc` only do generation on the left side of $=$ sign, it merely copies the code on the right hand  to the generated `hs` file. That's why we see function from `Language.Haskell.Rules` module appear in the `sem_Expr_List `. 
To modify `strategy.hs`, the recommand approach is to modify `strategy.ag` first, then generate `strategy.hs`. 
`uuagc` can be found via `cabal list` and install with `cabal install`


---
1. Code are published under difference licsense. when i move them around, regroup, modify. I still need to respect the license right?  -- all apache license 
2. `Syntax.hs` contains the custom implementation of Haskell language, it's neither `Helium` or `GHC`. Since we want to abandon it, we dont need to keep it anymore, right? So, is the `View` module. -- 
   we dont need View, or syntax.ag 
3. ```
   parse "fromBin" "[LvmLang.Int] -> LvmLang.Int" "fromBin [] = [] + []"
   ```
   This compile as the type signature gets thrown away, reason unknown.
   probably, we did it for a reason, let's keep it there
   and add a control flag to specify whether we ignore or not the type
  

--- 
1. In `compile` function, why do we have a process erase type signatures away?
Considering the following snippet 
```haskell
parse "fromBin" "[LvmLang.Int] -> LvmLang.Int" "fromBin [] = 1"
-- it gives back the following error messages
(1,13): Undefined type constructor "LvmLang.Int"
(1,29): Undefined type constructor "LvmLang.Int"
-- it could be easily resolved by removing the leading LvmLang prefix
```
This seesm easy, why dont we just remove the leading prefix, rather than erasing the type signature?
I suppose, its just a hypothesis, in `askelle` , we were actually reading from the cache file, where cache file is filled with `LvmLang`, and cache file is always automatically generated, it's computational intense and pointless to remove it every time. Also, if `Helium` fails to do `type inference` , it could leads to a conclusion that student's solution has flaws in itself.


--- 
1. when checking `heliumTestCases/correct/Reexport2` using helium frontend, it seems to me that our current approach cannot handle module imports other than the standard prelude. As checking `import <moduleName>` requires the existence of `<moduleName>.lvm` which is the compiled redentition of `<moduleName>.hs`.
2. The frontend seems cannot handle valid newline character in a string 
3. Helium doesn't support number overloading, thus types for `s,a,b` in `FailAsIntended/parser/contexts` are too general
4. How is `f (-.4.0) = 3` , quoted from `Success/parser/PatUnaryMinOk`, a valid piece of code? Maybe because is it restricted inside the front end?
