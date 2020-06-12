let
  /* pkgs = import <nixpkgs> { }; */
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2020-04-29";
    url = "https://github.com/nixos/nixpkgs-channels/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    ref = "refs/heads/nixos-unstable";
    rev = "7c399a4ee080f33cc500a3fda33af6fccfd617bd";
  }) {};

  compiler = "ghc883";

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: upser: rec {
      rdf4h = self.callCabal2nix "rdf4h" (builtins.fetchGit {
        url = "https://github.com/robstewart57/rdf4h.git";
        ref = "refs/heads/master";
        rev = "9658c9c361fb175f614a127c96da8ec42665334a";
      }) {};
      xsd = self.callCabal2nix "xsd" (builtins.fetchGit {
        url = "https://github.com/data61/xsd.git";
        ref = "refs/heads/master";
        rev = "2a429fd397d7b6a556a399b867dfe37cfefbc041";
      }) {};
    };
  };
in
  haskellPackages.callCabal2nix "cq2rdf" (./.) {
    cinetv4h = haskellPackages.callCabal2nix "cinetv4h" (./../cinetvdb/cinetv4h) {};
    rdf4h = pkgs.haskell.lib.dontCheck haskellPackages.rdf4h;
    xsd = pkgs.haskell.lib.dontCheck haskellPackages.xsd;
    # esqueleto = haskellPackages.callHackage "esqueleto" "3.0.0" {};
  }
