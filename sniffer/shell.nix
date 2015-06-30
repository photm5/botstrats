{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, aeson, base, network, stdenv }:
      mkDerivation {
        pname = "botstrats-sniffer";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ cabal-install aeson base network ];
        homepage = "https://github.com/shak-mar/botstrats";
        description = "Sniffer for botstrats";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
