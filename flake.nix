{
  description = "A very basic flake";
  nixConfig = {
    allow-import-from-derivation = "true";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          helloProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc884";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
              ];
              modules = [
               { packages.ghc-tcplugin-api.doHaddock = false; } 
              ];
              flake.variants = {
                ghc884.compiler-nix-name = pkgs.lib.mkForce "ghc884";
                ghc924.compiler-nix-name = pkgs.lib.mkForce "ghc924";
              };
          };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.helloProject.flake {
      };
    in flake // {
      # Built by `nix build .`
#      packages.default = flake.packages."segfault-repro:exe:segfault-repro";
    });
}
