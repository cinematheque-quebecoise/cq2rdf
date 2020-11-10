{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  inherit (pkgs) python38Packages;

  project = import ./release.nix;

  hdt = import ./nix/hdt.nix { inherit pkgs; };

  rdflib-hdt = import ./nix/rdflib-hdt.nix {
    inherit (python38Packages) buildPythonPackage;
    inherit (python38Packages) fetchPypi;
    inherit (pkgs.stdenv) lib;
    inherit (python38Packages) rdflib;
    inherit (python38Packages) pybind11;
  };

  bootstrapBlazegraph  = import ./scripts/bootstrapBlazegraph.nix { inherit pkgs; };
  testSparqlQueries  = import ./scripts/test_sparql_queries.nix { inherit pkgs; };
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.shelltestrunner
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    haskellPackages.brittany

    pkgs.nix
    pkgs.curl
    pkgs.jq
    pkgs.git
    pkgs.zlib
    pkgs.glibcLocales

    python38Packages.pyyaml
    python38Packages.tabulate

    hdt
    rdflib-hdt

    bootstrapBlazegraph
    testSparqlQueries
  ];

  LANG = "en_US.UTF-8";

  shellHook = ''
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
  '';
}
