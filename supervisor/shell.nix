{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, base, bytestring, directory, filepath, mtl
      , network, process, stdenv, transformers, unix, uuid
      }:
      mkDerivation {
        pname = "botstrats-supervisor";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ cabal-install
          base bytestring directory filepath mtl network process transformers
          unix uuid
        ];
        homepage = "https://github.com/shak-mar/botstrats";
        description = "Supervisor for botstrats";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
