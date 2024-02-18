
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
4. How is `f (-.4.0) = 3` , quoted from `Success/parser/PatUnaryMinOk`, a valid piece of code? Maybe it's valid in the realm of front end?



---
1. when generating core code, it may contain some text like `\\226\\128\\162`. Texts like this are the UTF-8 decimal representations of unicode characters.  [Unicode/UTF8 lookup table](https://www.utf8-chartable.de/unicode-utf8-table.pl?start=8192&number=128&utf8=dec) can come in handy
   1.  `\\226\\128\\162` is  • (Bullet)
   2.  `\\126\\128\\152` is  ‘ (Left single quotation mark)
   3.  `\\226\\128\\153` is ’ (Right  single quotation mark)
Example
````
"Main.hs:12:14: error:\n\
\    \\226\\128\\162 Found hole: _ :: a\n\
\      Where: \\226\\128\\152a\\226\\128\\153 is a rigid type variable bound by\n\
\               the type signature for:\n\
\                 identity :: forall a. Num a => a -> a\n\
\               at Main.hs:11:1-39\n\
\    \\226\\128\\162 In the first argument of \\226\\128\\152(+)\\226\\128\\153, namely \\226\\128\\152_\\226\\128\\153\n\
\      In the expression: _ + x\n\
\      In an equation for \\226\\128\\152identity\\226\\128\\153:\n\
\          identity x\n\
\            = _ + x\n\
\            where\n\
\                y :: a\n\
\                y = 1\n\
\    \\226\\128\\162 Relevant bindings include\n\
\        y :: a (bound at Main.hs:15:3)\n\
\        x :: a (bound at Main.hs:12:10)\n\
\        identity :: a -> a (bound at Main.hs:12:1)\n\
\      Constraints include Num a (from Main.hs:11:1-39)\n\
\      Valid hole fits include\n\
\        x :: a (bound at Main.hs:12:10)\n\
\        y :: a (bound at Main.hs:15:3)\n\
\(deferred type error)"#

more egonormic 

"Main.hs:12:14: error:\n\
\    • Found hole: _ :: a\n\
\      Where: ‘a’ is a rigid type variable bound by\n\
\               the type signature for:\n\
\                 identity :: forall a. Num a => a -> a\n\
\               at Main.hs:11:1-39\n\
\    • In the first argument of ‘(+)’, namely ‘_’\n\
\      In the expression: _ + x\n\
\      In an equation for ‘identity’:\n\
\          identity x\n\
\            = _ + x\n\
\            where\n\
\                y :: a\n\
\                y = 1\n\
\    • Relevant bindings include\n\
\        y :: a (bound at Main.hs:15:3)\n\
\        x :: a (bound at Main.hs:12:10)\n\
\        identity :: a -> a (bound at Main.hs:12:1)\n\
\      Constraints include Num a (from Main.hs:11:1-39)\n\
\      Valid hole fits include\n\
\        x :: a (bound at Main.hs:12:10)\n\
\        y :: a (bound at Main.hs:15:3)\n\
\(deferred type error)"#


````


--- 
A possible speed up 
Right, we access uniplate functions for `CoreProgram` via `Data` instance. 
According to the hoogle page, by adding `Uniplate` instance supports, functions could have 5x perfromance. <- dont think this is possibile any more, as this requires contributing to GHC directly, or do orphan instances work in this case? 


--- 
1. Did Matilda consider using `deBruijnize` when writing her own Similar instance?
2. What's wrong with the error in `BinderEquality.hs`?
3. Should we consider running `inlineBinds` multiple times?
4. What's the ideal way to handle impossible case? 

