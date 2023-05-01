# Ask2elle
Ask2Elle is an intelligent programming tutor for Haskell

# Dependencies

## Tools
We recommend using `nix`, the package manager, for installing all the necessary tools to build the project. Simply type `nix develop` in your terminal to get started.

We understand that some of you may have reservations about using it. For those individuals, we have provided an exclusive list of dependencies required to build the project. This list is sufficient for the project's construction and can be used as an alternative.
```bash
bashInteractive
cabal-install
fd
gnumake
ghc 
```

## Local Dependencies 
Currently, the following steps must be performed manually to build the project. Our goal is to automate steps 1-4 by adding flake support to `lvm`, `helium`, `Top`, and `ideas`:

1. Add `~/.cabal/bin` to your PATH. 
2. Create a folder and clone the following repositories:
   - [askelle](https://github.com/alexgerdes/askelle), use the hardcoded-exercises branch
   - [lvm](https://github.com/alexgerdes/lvm)
   - [helium](https://github.com/alexgerdes/helium)
   - [Top](https://github.com/alexgerdes/Top)
   - [ideas](https://github.com/ideas-edu/ideas), use the ideas-bastiaan branch
3. Create a `cabal.project` file with the following snippet:
   ```cabal
   packages: helium/
             lvm/
             Top/
             ideas/
             askelle/
   ```
     This tells cabal to use local packages for building projects instead of fetching them from Hackage.

4. Execute the following commands:
    ```bash
    cabal install lvm --overwrite-policy=always 
    cabal install Top --overwrite-policy=always 
    cabal install helium --overwrite-policy=always 
    cabal install askelle --overwrite-policy=always
    ```
5. Run `heliumpath` in the shell and execute `make` in the corresponding `share/lib` directory.
6. Navigate to the askelle directory and run `askelle.cgi --all-scripts`. This generates files in the scripts folder.
7. The environment is now set up.




