{ pkgs ? (import <nixpkgs> {}) }:
with pkgs; stdenv.mkDerivation {
  name = "shell";
  buildInputs = [ ncat ];
}
