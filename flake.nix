{
  description = "Ask2elle-Development-Environment";
  inputs = { nixpkgs-master.url = "github:NixOS/nixpkgs/nixos-unstable"; };

  outputs = inputs@{ self, nixpkgs-master, ... }:
    let
      # GENERAL
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = nixpkgs-master.lib.genAttrs supportedSystems;
      nixpkgsFor = system: nixpkgs-master.legacyPackages.${system};

      mkDevEnv = system:
        let pkgs = nixpkgsFor system;
        in pkgs.stdenv.mkDerivation {
          name = "Standard-Dev-Environment-with-Utils";
          buildInputs = (with pkgs; [
            bashInteractive
            cabal-install
            fd
            git
            gnumake
            nixfmt
            zlib
            sqlite
            # gcc
          ]);
        };

      haskell = rec {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            stdDevEnv = mkDevEnv system;
            haskell-pkgs = pkgs.haskellPackages;
            project = (pkgs.stdenv.mkDerivation {
              name = "Haskell-Dev-Environment-with-Utils";
              buildInputs = [
                pkgs.haskell.compiler.ghc928
                pkgs.haskell.packages.ghc928.haskell-language-server
              ] ++ stdDevEnv.buildInputs
                ++ (with haskell-pkgs; [ cabal-fmt fourmolu ]);
              shellHook = ''
                export PATH=~/.cabal/bin:$PATH;
              '';
            });

          in project;
      };
                     #  export LD_LIBRARY_PATH=${pkgs.gcc.cc.lib}/lib:$LD_LIBRARY_PATH
      debugGHC = rec {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            stdDevEnv = mkDevEnv system;
            haskell-pkgs = pkgs.haskellPackages;
            project = (pkgs.stdenv.mkDerivation {
              name = "Haskell-Dev-Environment-with-Utils";
              buildInputs = stdDevEnv.buildInputs ++ (with pkgs; [
                # needed for running cabal with customized ghc
                glibc
                glib 
                gmp 
                numactl 
                ncurses 
              ]) ++ (with haskell-pkgs; [ cabal-fmt fourmolu ]);
              shellHook = ''
                export PATH=~/.cabal/bin:$PATH;
                export PATH=~/Desktop/code/alexgerdes/ghc/_build/stage1/bin:$PATH;
              '';
            });

          in project;
      };

    in {
      debugGHC = perSystem (system: (debugGHC.projectFor system));
      haskell = perSystem (system: (haskell.projectFor system));
      devShells = perSystem (system: {
        default = self.haskell.${system};
        debugGHC = self.debugGHC.${system};
      });
      packages = perSystem (system: { default = self.haskell.${system}; });
    };
}
