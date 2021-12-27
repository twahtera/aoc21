{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          haskell-language-server split map multiset MissingH extra array containers hmatrix mtl
  ]);

in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
