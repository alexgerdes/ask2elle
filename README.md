# Ask2elle
Ask2Elle is an intelligent programming tutor for Haskell

# Dependencies

## Tools
We recommend using `nix`, the package manager, to install all the essential tools for building the project. To begin, enable [nix flakes](https://nixos.wiki/wiki/Flakes) option and enter `nix develop` in your terminal.

We recognize that some of you might be hesitant to use nix. For those who prefer an alternative, the following list consists of required dependencies for the project. It is adequate for building the project and can be used as a substitute.
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
1. Add "~/.cabal/bin" to the path 
2. Run `heliumpath` in the shell and execute `make` in the corresponding `share/lib` directory.
3. Navigate to the askelle directory and run `askelle.cgi --all-scripts`. This generates files in the scripts folder.
4. The environment is now set up.


## Dockerfile
Right now, the dockerfile automatically runs the testsuite. 

Useful commands for playing with the docker
1. 
   Inside ask2elle directory, run the following command, create an image with the name ask2elle-image.
   `GIT_USERNAME` and `GIT_TOKEN` need to be substituted with your own github username and personal access token. 
   ```docker
   docker build -t ask2elle-image --build-arg GIT_USERNAME=xxxxx --build-arg GIT_TOKEN=yyy .
   ```
2. 
   Fire up a container, named as `ask2elle-container`, using `ask2elle-image` 
   The `-t` flag allows the container continues to run, even though actions described in the dockerfile completed 
   ```docker 
   docker run -t --name ask2elle-container ask2elle-image
   ```

3. Hook up a local terminal with the `ask2elle-container` terminal
   ```docker
   docker exec -it ask2elle-container bash
   ```

## Entry Point for Ask2elle
`parameterizedCompSimplNormalized` from module `GhcLib.Compile.Compile` is the entry point. It takes a list of `NormalizationOption` and `PostNormalizationOption` from module `GhcLib.Transform.Transform`. You can find the utility of each data constructor in the corresponding markdown file. 

## Some commands 
`cabal run ask2elle` currently generate a csv file in which contains the matching ratio for every possible combination of `NormalizationOption` and `PostNormalizationOption`. Unfortunately, since the complexity of every possible combination is notoriously large and haskell is a lazy language, 16Gib Ram laptop cannot handle the amount of chunks created. And i'm too lazy to denote BangPatterns everywhere, `Strict` progma helps generating a coverage test and printing it out in `table.csv`.

The following is a part of `table.csv`. 
The number in the first row mark each run with an index 
Under the associative index, we can know the order of performed normalization options. Both Run 1 and Run 2 run perform exactly the same options and in the same order. Run 2 differs from Run 1, by not using `RemoveTyEvidence` from `PostNormalizationOption`. In the Ratio, we can clearly see that `RemoveTyEvidence` brings significant unification ratio.

For row `comparisonCount`, `matchedCount` and `unmatchedCount`. `comparisonCount` with $19$ means that we have total $19$ student solutions, `matchedCount` means there are $16$ of them match with at least one correct solutions. `ratio` is calculated by dividing `matchedCount` with `comparisonCount`.


|                  | 1   | 2   |
|:-----------------|:----|:----|
| InlineBinds      | 1   | 1   |
| RecToLetRec      | 2   | 2   |
| RemoveEqCheck    | 3   | 3   |
| EtaReduce        | 4   | 4   |
| RemoveTyEvidence | 1   |     |
| comparisonCount  | 19  | 19  |
| matchedCount     | 16  | 2   |
| unmatchedCount   | 3   | 17  |
| ratio            | 84% | 10% |



`cabal test` currently run the testsuite for `Helium` Compiler. There is no testsuite for `ask2elle` yet. 

