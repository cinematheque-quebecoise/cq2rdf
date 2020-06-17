{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;

  hdt = import ./hdt.nix { inherit pkgs; };
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    pkgs.zlib
    pkgs.glibcLocales
    haskellPackages.shelltestrunner
    hdt
  ];

  LANG = "en_US.UTF-8";
}
