{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, base, bytestring, mtl, network, random
      , stdenv, transformers, uuid
      }:
      mkDerivation {
        pname = "botstrats";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ cabal-install
          base bytestring mtl network random transformers uuid
        ];
        homepage = "https://github.com/shak-mar/botstrats";
        description = "A strategy game that makes you program robots";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
