# How to build customized GHC 9.2.8 locally for testing purpose

1. following the [insturctions](https://gitlab.haskell.org/ghc/ghc/-/wikis/building) here(mainly git clone and cd to the ghc folder)
2. run `git checkout tags/ghc-9.2.8-release -b ghc-9.2.8` which creates a locally branch call `ghc-9.2.8`, followed by `git submodule update --init --recursive`
3. Fullfilling the requirement for building the ghc. Nix users scroll down
4. After fullfilling prerequisites for building the project
5. run `./boot && configure_ghc` followed by `./hadrian/build -j` as indicated in the build page
6. It would take roughly half a hour to finish up the building
7. export `_build/stage1/bin/` in your path, such that cabal could locate ghc 
8. you are good to go
  

It's easy for nix user for setting up the development environment
1. git clone [ghc.nix](https://github.com/alpmestan/ghc.nix) inside `ghc` folder
2. Modify the `ghc.nix`, change the entry `bootghc ? "ghc96"` at line 17 to  `"bootghc ? "ghc92"` <- because the base package version used by ghc96 would conflict dependencies for building ghc928 
3. you could choose either manually `nix develop` or running fancy `direnv`, i prefer the first option
4. back to step 4 above

