```haskell
module Test where 
times2 :: forall a. (Num a) => a -> a
times2 x = _ + x
	where
	y :: a
	y = 1
```

```bash
ghc -ddump-simpl Test.hs
```

Fail to compile, output in shell

````bash
[1 of 1] Compiling Test  ( Test.hs, Test.o )                                               
Test.hs:10:14: error:
	• Found hole: _ :: a
	  Where: ‘a’ is a rigid type variable bound by
			   the type signature for:
				 identity :: forall a. Num a => a -> a
			   at Test.hs:9:1-39
	• In the first argument of ‘(+)’, namely ‘_’
	  In the expression: _ + x
	  In an equation for ‘identity’:
		  identity x
			= _ + x
			where
				y :: a
				y = 1
	• Relevant bindings include
		y :: a (bound at Test.hs:13:3)
		x :: a (bound at Test.hs:10:10)
		identity :: a -> a (bound at Test.hs:10:1)
	  Constraints include Num a (from Test.hs:9:1-39)
	  Valid hole fits include
		x :: a (bound at Test.hs:10:10)
		y :: a (bound at Test.hs:13:3)
   |
10 | identity x = _ + x
   |
````



```bash
ghc -ddump-simpl -fdefer-typed-holes Test.hs
```



Successful Compilation, as `ghc` defers type holes errors into warning

```bash  
[1 of 1] Compiling Test             
( Test.hs, Test.o )                                                                 Test.hs:10:14: warning: [-Wtyped-holes]
    • Found hole: _ :: a
      Where: ‘a’ is a rigid type variable bound by
               the type signature for:
                 identity :: forall a. Num a => a -> a
               at Test.hs:9:1-39
    • In the first argument of ‘(+)’, namely ‘_’
      In the expression: _ + x
      In an equation for ‘identity’:
          identity x
            = _ + x
            where
                y :: a
                y = 1
    • Relevant bindings include
        y :: a (bound at Test.hs:13:3)
        x :: a (bound at Test.hs:10:10)
        identity :: a -> a (bound at Test.hs:10:1)
      Constraints include Num a (from Test.hs:9:1-39)
      Valid hole fits include
        x :: a (bound at Test.hs:10:10)
        y :: a (bound at Test.hs:13:3)
   |
10 | identity x = _ + x
   |              ^

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 24, types: 18, coercions: 0, joins: 0/0}

-- RHS size: {terms: 9, types: 8, coercions: 0, joins: 0/0}
identity :: forall a. Num a => a -> a
[GblId, Arity=2, Unf=OtherCon []]
identity
  = \ (@a_ay1) ($dNum_ay2 :: Num a_ay1) (x_aj0 :: a_ay1) ->
      + @a_ay1
        $dNum_ay2
        (case Control.Exception.Base.typeError
                @GHC.Types.LiftedRep
                @()
                "Test.hs:10:14: error:\n\
                \    \\226\\128\\162 Found hole: _ :: a\n\
                \      Where: \\226\\128\\152a\\226\\128\\153 is a rigid type variable bound by\n\
                \               the type signature for:\n\
                \                 identity :: forall a. Num a => a -> a\n\
                \               at Test.hs:9:1-39\n\
                \    \\226\\128\\162 In the first argument of \\226\\128\\152(+)\\226\\128\\153, namely \\226\\128\\152_\\226\\128\\153\n\
                \      In the expression: _ + x\n\
                \      In an equation for \\226\\128\\152identity\\226\\128\\153:\n\
                \          identity x\n\
                \            = _ + x\n\
                \            where\n\
                \                y :: a\n\
                \                y = 1\n\
                \    \\226\\128\\162 Relevant bindings include\n\
                \        y :: a (bound at Test.hs:13:3)\n\
                \        x :: a (bound at Test.hs:10:10)\n\
                \        identity :: a -> a (bound at Test.hs:10:1)\n\
                \      Constraints include Num a (from Test.hs:9:1-39)\n\
                \      Valid hole fits include\n\
                \        x :: a (bound at Test.hs:10:10)\n\
                \        y :: a (bound at Test.hs:13:3)\n\
                \(deferred type error)"#
         of wild_00 {
         })
        x_aj0

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule1_ryO :: GHC.Prim.Addr#
[GblId, Unf=OtherCon []]
$trModule1_ryO = "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule2_ryP :: GHC.Types.TrName
[GblId, Unf=OtherCon []]
$trModule2_ryP = GHC.Types.TrNameS $trModule1_ryO

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule3_ryQ :: GHC.Prim.Addr#
[GblId, Unf=OtherCon []]
$trModule3_ryQ = "Test"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule4_ryR :: GHC.Types.TrName
[GblId, Unf=OtherCon []]
$trModule4_ryR = GHC.Types.TrNameS $trModule3_ryQ

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
Test.$trModule :: GHC.Types.Module
[GblId, Unf=OtherCon []]
Test.$trModule = GHC.Types.Module $trModule2_ryP $trModule4_ryR


```

Reminder
	For texts like, `\\226\\128\\162`, are the UTF-8 decimal representations of unicode characters.
		1. `\\226\\128\\162` is • (Bullet)
		2. `\\126\\128\\152` is ‘ (Left single quotation mark)
		3. `\\226\\128\\153` is ’ (Right single quotation mark)

For better readability, we substitute UTF-8 representations with unicode characters.


The output contains unwanted info, like `$trModule1_ryO`. They are used by `typeable` to get type information in runtime, not needed in our case. And we don't want to read the warning every time, hence we need to find a way to suppress the warning.

```bash
ghc -ddump-simpl -fdefer-typed-holes -Wno-typed-holes -dno-typeable-binds Test.hs
```

huh?!! Nothing shows up in the command line. What's going on?
GHC don't think the flags we just added would cause a significant change in the output, it refuses to compile the code. The flag `-fforce-recomp` ameliorates the problem.

```bash
ghc -ddump-simpl -fdefer-typed-holes -Wno-typed-holes -dno-typeable-binds -fforce-recomp Test.hs
```

Now the output
```bash
[1 of 1] Compiling Test             ( Test.hs, Test.o )

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 10, types: 13, coercions: 0, joins: 0/0}

-- RHS size: {terms: 9, types: 8, coercions: 0, joins: 0/0}
identity :: forall a. Num a => a -> a
[GblId, Arity=2, Unf=OtherCon []]
identity
  = \ (@a_ay1) ($dNum_ay2 :: Num a_ay1) (x_aj0 :: a_ay1) ->
      + @a_ay1
        $dNum_ay2
        (case Control.Exception.Base.typeError
                @GHC.Types.LiftedRep
                @()
                "Test.hs:10:14: error:\n\
                \    • Found hole: _ :: a\n\
                \      Where:  ‘ a ’ is a rigid type variable bound by\n\
                \               the type signature for:\n\
                \                 identity :: forall a. Num a => a -> a\n\
                \               at Test.hs:9:1-39\n\
                \    • In the first argument of  ‘ (+) ’, namely  ‘ _ ’\n\
                \      In the expression: _ + x\n\
                \      In an equation for  ‘ identity ’:\n\
                \          identity x\n\
                \            = _ + x\n\
                \            where\n\
                \                y :: a\n\
                \                y = 1\n\
                \    • Relevant bindings include\n\
                \        y :: a (bound at Test.hs:13:3)\n\
                \        x :: a (bound at Test.hs:10:10)\n\
                \        identity :: a -> a (bound at Test.hs:10:1)\n\
                \      Constraints include Num a (from Test.hs:9:1-39)\n\
                \      Valid hole fits include\n\
                \        x :: a (bound at Test.hs:10:10)\n\
                \        y :: a (bound at Test.hs:13:3)\n\
                \(deferred type error)"#
         of wild_00 {
         })
        x_aj0
```



We want our command to be explicit, i.e. explicitly declaring every possible flags that have potentials to modify the output.

From the above snippet, 

we can suppress line `Constraints include Num a (from Test.hs:9:1-39)` with `-fno-show-hole-constraints`

we can suppress line `(bound at Test.hs:10:10)` with `-fno-show-provenance-of-hole-fits`

we can suppress hole-fit type with `-fno-show-type-of-hole-fits`, the output becomes
```
	...
    Valid hole fits include\n\
	    x :: a (bound at Test.hs:10:10)\n\
	...
```

we can suppress the whole valid hole fits section with `-fno-show-valid-hole-fits`

Consider another example 

```haskell
module Test where

import Data.List

findElem :: (Eq a) => a -> [a] -> Maybe a
findElem x xs = _ (== x) xs
```

Given the command, 
```bash
ghc -ddump-simpl -fdefer-typed-holes -Wno-typed-holes -dno-typeable-binds -fforce-recomp -fshow-valid-hole-fits  Test.hs
```


we get 
```bash
    ...
    "Test.hs:12:17: error:\n\
    \     •  Found hole: _ :: (a -> Bool) -> [a] -> Maybe a\n\
    \      Where: ‘a ’ is a rigid type variable bound by\n\
    \               the type signature for:\n\
    \                 findElem :: forall a. Eq a => a -> [a] -> Maybe a\n\
    \               at Test.hs:11:1-41\n\
    \     •  In the expression: _ (== x) xs\n\
    \      In an equation for ‘findElem ’: findElem x xs = _ (== x) xs\n\
    \     •  Relevant bindings include\n\
    \        xs :: [a] (bound at Test.hs:12:12)\n\
    \        x :: a (bound at Test.hs:12:10)\n\
    \        findElem :: a -> [a] -> Maybe a (bound at Test.hs:12:1)\n\
    \      Constraints include Eq a (from Test.hs:11:1-41)\n\
    \      Valid hole fits include\n\
    \        find :: forall (t :: * -> *) a.\n\
    \                Foldable t =>\n\
    \                (a -> Bool) -> t a -> Maybe a\n\
    \          with find @[] @a\n\
    \(deferred type error)"#
    ...
```

with an extra flag `-fshow-docs-of-hole-fits`, we can get the description about the possible type hole.
```bash
    "
    \        find :: forall (t :: * -> *) a.\n\
    \                Foldable t =>\n\
    \                (a -> Bool) -> t a -> Maybe a\n\
    \          with find @[] @a\n\
    \    -- | The 'find' function takes a predicate and a structure and returns\n\
         \-- the leftmost element of the structure matching the predicate, or\n\
         \-- 'Nothing' if there is no such element.\n\
         \--\n\
         \-- ==== __Examples__\n\
         \--\n\
         \-- Basic usage:\n\
         \--\n\
         \-- >>> find (> 42) [0, 5..]\n\
         \-- Just 45\n\
         \--\n\
         \-- >>> find (> 12) [1..7]\n\
         \-- Nothing\n\
         \          with find @[] @a\n\
    "
    ...
```


```bash
"
    \      Valid hole fits include\n\
    \        find :: forall (t :: * -> *) a.\n\
    \                Foldable t =>\n\
    \                (a -> Bool) -> t a -> Maybe a\n\
    \          with find @[] @a\n\
"
```

Notice `find @[] @a`? It can be suppressed by flag  `-fno-show-type-app-of-hole-fits` . Thus it becomes

```bash
"
    \      Valid hole fits include\n\
    \        find :: forall (t :: * -> *) a.\n\
    \                Foldable t =>\n\
    \                (a -> Bool) -> t a -> Maybe a\n\
"
```

There is another flag `-fshow-type-app-vars-of-hole-fits` which alter the output to type equivalence style rather than type application style.
```bash
"
	 \      Valid hole fits include\n\
	 \        find :: forall (t :: * -> *) a.\n\
	 \                Foldable t =>\n\
	 \                (a -> Bool) -> t a -> Maybe a\n\
	 \          with t ~ [], a ~ a\n\
"
```

WARNING
	These two flags are not compatible with each other. The order of their appearance ,the latter one ,determines the output given by GHC.

Flags `-no-keep-hi-files` and `-no-keep-o-files` instruct ghc not to store intermediate files on disk, save some IO actions


Ultimate command 
```bash
 ghc  -fforce-recomp -ddump-simpl -fdefer-typed-holes -Wno-typed-holes -dno-typeable-binds  -fshow-valid-hole-fits -fshow-hole-constraints -fshow-provenance-of-hole-fits -fshow-type-of-hole-fits -fshow-docs-of-hole-fits -fshow-type-app-vars-of-hole-fits -no-keep-hi-files -no-keep-o-files Test.hs
```

 
