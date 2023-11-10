# How to build GHC 9.2.8 locally for nixos(nix flake enabled)

1. following the [insturctions](https://gitlab.haskell.org/ghc/ghc/-/wikis/building) here 
2. run `git checkout tags/ghc-9.2.8-release -b ghc-9.2.8` which creates a locally branch call `ghc-9.2.8`
3. git clone [ghc.nix](https://github.com/alpmestan/ghc.nix)
4. Modify the `ghc.nix`, change the entry `bootghc ? "ghc96"` at line 17 to  `"bootghc ? "ghc92"` <- because the base package version used by ghc96 would conflict dependencies for building ghc928 
   