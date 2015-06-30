{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, base, gloss, network, stdenv, transformers }:
      mkDerivation {
        pname = "botstrats-visualizer";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ cabal-install base gloss network transformers ];
        homepage = "https://github.com/shak-mar/botstrats";
        description = "Visualizer for botstrats, but can be used for other things";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
