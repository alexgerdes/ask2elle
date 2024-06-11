
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
2. ~~What's wrong with the error in `BinderEquality.hs`~~
3. Should we consider running `inlineBinds` multiple times?
4. What's the ideal way to handle impossible case? 


--- 2024/03/10
1. I think we need to disable `build` optimization  
2. Type equality check is rough, need a second eye


--- 2024/03/24
1. [x] I need a function that runs every student solution against model solutions and do a summary 
   1. Summary in what format? What library is better for this purpose?
   2. Where should i put this function?
2. [x]  I need a function that try all possible normalizations and give a summary. 
3. [x] A possible speedup for the testsuite of checking the validity of holemapping is load all model solutions upfront, even though the student solution could possibly not pass type checking. 
4. [ ] Why is `analyzeAll` in `TestHoleMapping` so slow? Maybe we fire up a ghc instance for every file? 
   1. for instance, 5 model solutions, and 10 student solutions. we fire up 15 fifteen times 
5. [ ] check if there exists a flags for disabling builder/foldr 
6. [x] look for proper way for type equivalence 
   1. At least right now, it is not doing string comparison in string  
7. [ ] discarding type evidence is not ideal, look for some unifiication solutions.
   1. It turns out removing type evidence brings the most matched solutions
   2. But still need an unification solution
8. [ ] write a function that check when non-similar problem start to diverges  

--- 2024/04/10
1. [x] an automatic testsuite
   1. [ ] the program space leaks everywhere
   2. [ ] the program have too many embarrasment parallelism 
   
Notes :
1. The transformation `RemoveTyEvidence` cannot be arbitrarily interleaved with other transformations, as it results in an invalid core representation 
   1. If it is an unification algorithm, can it be interleaved with other transformation?
2. The transformation `alphaRenaming` must be used in the last stage, prior to `RemoveTyEvidence`. 

-- 2024/04/22 
1. The ghc pipeline now takes a list of targets, increasing the testsuite significant.
2. However, we cannot still run a full testsuite, but more than last time, as it still consumes too much memory.
3. Lol, the equality check for type is not irrelvent. because we remove the typing information all after postNormalization techniques
   
cabal run ask2elle -- +RTS -N -p -s -l -hT -i0.5 -RTS
-- -N for concurrency 
-- -p produces a standard time profile report ( the .prof file)
-- -s produces a more detailed summary at the end of the program
-- -l produces the eventlog file 
-- -hT  Generates a basic heap profile, in the file prog.hp
 eventlog2html ask2elle.eventlog -o ask2elle.eventlog.html


# Weekly concers
1. [ ] How to tackle space leaks in ask2elle
2. [ ] Model solutions are expected to be well-typed. 