let
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    # name = "nixos-unstable-2020-04-29";
    url = "https://github.com/NixOS/nixpkgs/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    ref = "refs/heads/nixos-20.09";
    rev = "da7f4c4842520167f65c20ad75ecdbd14e27ae91";
  }) {};

  inherit (pkgs) haskellPackages;
  inherit (pkgs) python38Packages;

  project = import ./release.nix;

  hdt = import ./nix/hdt.nix { inherit pkgs; };

  rdflib-hdt = import ./nix/rdflib-hdt.nix {
    inherit (python38Packages) buildPythonPackage;
    inherit (python38Packages) fetchPypi;
    inherit (pkgs) lib;
    inherit (python38Packages) rdflib;
    inherit (python38Packages) pybind11;
  };

  bootstrapBlazegraph  = import ./scripts/bootstrapBlazegraph.nix { inherit pkgs; };
  startBlazegraph  = import ./scripts/startBlazegraph.nix { inherit pkgs; };
  testSparqlQueries  = import ./scripts/test_sparql_queries.nix { inherit pkgs; };
  generateVoidDataset  = import ./scripts/generateVoidDataset.nix { inherit pkgs; };

  cinetv-version = "0.1.0";

  cinetv-assets-json = builtins.fromJSON (builtins.readFile (pkgs.fetchurl {
    url = "https://gitlab.com/api/v4/projects/19038139/releases/v${cinetv-version}/assets/links";
    # sha256 = "1ybilp0p21b8wnzgm8gp2a7wr48amac4ax6csbb9hvfcxixgxcix";
    sha256 = "PbL+euzMbZjW0sx0RZiqCpHMjxL3ofq+5WgFccGlcfk=";
  }));

  cinetv-sqlite = fetchTarball ((builtins.head (builtins.filter (v: v.name == "cinetv-2019-07-12-sqlite.tar.gz") cinetv-assets-json)).direct_asset_url);
  cinetv-csv = fetchTarball ((builtins.head (builtins.filter (v: v.name == "cinetv-2019-07-12-csv.tar.gz") cinetv-assets-json)).direct_asset_url);
  # cinetv-csv = builtins.builtins.fromJSON (builtins.readFile cinetv-assets-file);

  # src = pkgs.fetchurl {
  #   owner = "cinematheque-quebecoise";
  #   repo = "cinetvdb";
  #   rev = "v0.1.0";
  #   sha256 = "06p8skfm0j88yf9njaqzym6xz985ahapqbviwl3lzm4iyipc05mf";
  # };

in
pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.shelltestrunner
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    haskellPackages.brittany
    haskellPackages.haskell-language-server

    pkgs.curl
    pkgs.cacert
    pkgs.jq
    pkgs.git
    pkgs.zlib
    pkgs.glibcLocales

    python38Packages.requests
    python38Packages.pyyaml
    python38Packages.tabulate

    hdt
    rdflib-hdt

    bootstrapBlazegraph
    startBlazegraph
    testSparqlQueries
    generateVoidDataset
  ];

  LANG = "en_US.UTF-8";

  CINETV_CSV_FILES_PATH = "${cinetv-csv}";
  CINETV_SQLITE_DB_PATH = builtins.toPath "${cinetv-sqlite}/*.db";
}
