{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "monoid-insertleft";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [ pkgs.haskellPackages.base ];
  homepage = "https://hackage.haskell.org/package/monoid-insertleft";
  description = "Some extension to the Foldable and Monoid classes";
  license = pkgs.lib.licenses.mit;
}
