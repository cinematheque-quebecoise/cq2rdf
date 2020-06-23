{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  inherit (pkgs) python35Packages;

  project = import ./release.nix;

  hdt = import ./hdt.nix { inherit pkgs; };
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    pkgs.nix
    pkgs.curl
    pkgs.jq
    pkgs.git
    pkgs.zlib
    pkgs.glibcLocales
    haskellPackages.shelltestrunner
    hdt
  ];

  LANG = "en_US.UTF-8";

  shellHook = ''
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
  '';
}
