{ pkgs ? (import <nixpkgs> {}) }:
with pkgs; stdenv.mkDerivation {
  name = "shell";
  buildInputs = [ gnumake inkscape imagemagick ];
}
