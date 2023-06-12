{
  description = "Ask2elle-Development-Environment";
  inputs = { nixpkgs-master.url = "github:NixOS/nixpkgs/master"; };

  outputs = inputs@{ self, nixpkgs-master, ... }:
    let
      # GENERAL
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = nixpkgs-master.lib.genAttrs supportedSystems;
      nixpkgsFor = system: nixpkgs-master.legacyPackages.${system};

      mkDevEnv = system:
        let
          pkgs = nixpkgsFor system;
          hls = pkgs.haskell-language-server.override {
            supportedGhcVersions = [ "92" ];
          };
        in pkgs.stdenv.mkDerivation {
          name = "Standard-Dev-Environment-with-Utils";
          buildInputs = [ pkgs.ghc hls ] ++ (with pkgs; [
            bashInteractive
            cabal-install
            fd
            git
            gnumake
            nixfmt
            zlib
            sqlite
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
              buildInputs = stdDevEnv.buildInputs
                ++ (with haskell-pkgs; [ cabal-fmt fourmolu ]);
              shellHook = ''
                export PATH=~/.cabal/bin:$PATH;
              '';

            });

          in project;
      };
    in {

      haskell = perSystem (system: (haskell.projectFor system));
      devShells = perSystem (system: { default = self.haskell.${system}; });
      packages = perSystem (system: { default = self.haskell.${system}; });
    };
}
