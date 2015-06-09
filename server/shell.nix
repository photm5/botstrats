with (import <nixpkgs> {}).pkgs;
let
  ghc = haskellPackages.ghcWithPackages (p: with p; [network uuid parsec]);
in
stdenv.mkDerivation {
  name = "botstrats-server-haskell-env";
  buildInputs = [ ghc haskellPackages.cabal-install ];
  shellHook = "eval $(grep export ${ghc}/bin/ghc)";
}
