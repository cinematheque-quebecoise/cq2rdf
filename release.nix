let
  /* pkgs = import <nixpkgs> { }; */
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    # name = "nixos-unstable-2020-04-29";
    url = "https://github.com/nixos/nixpkgs-channels/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    ref = "refs/heads/nixos-unstable";
    rev = "7c399a4ee080f33cc500a3fda33af6fccfd617bd";
  }) {};

  hdt = import ./nix/hdt.nix { inherit pkgs; };

  compiler = "ghc883";

  cinetvdb = builtins.fetchGit {
    url = "https://gitlab.com/cinematheque-quebecoise/cinetvdb.git";
    ref = "refs/heads/master";
  };

  rdf4hPackage = pkgs.haskellPackages.callCabal2nix "rdf4h" (builtins.fetchGit {
    url = "https://github.com/robstewart57/rdf4h.git";
    ref = "refs/heads/master";
    # rev = "018755800c6503ddebe27efb7261be1c95ee2f12";
    rev = "c71bca655508f9bedc6b9dd804994c866f17587b";
  }) {};

  hsparqlPackage = pkgs.haskellPackages.callCabal2nix "hsparql" (builtins.fetchGit {
    url = "https://github.com/robstewart57/hsparql.git";
    ref = "refs/heads/master";
    rev = "ac11fa787aa4317675d34ccb0009b7cda8b87550";
  }) {
    rdf4h = pkgs.haskell.lib.dontCheck rdf4hPackage;
  };

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: upser: rec {
      rdf4h = rdf4hPackage;

      hsparql = pkgs.haskell.lib.dontCheck hsparqlPackage;

      xsd = self.callCabal2nix "xsd" (builtins.fetchGit {
        url = "https://github.com/data61/xsd.git";
        ref = "refs/heads/master";
        rev = "2a429fd397d7b6a556a399b867dfe37cfefbc041";
      }) {};
      cinetv4h = self.callCabal2nix "cinetv4h" ("${cinetvdb}/cinetv4h") {};
    };
  };

  cq2rdf = haskellPackages.callCabal2nix "cq2rdf" (./.) {
    rdf4h = pkgs.haskell.lib.dontCheck haskellPackages.rdf4h;
    hsparql = pkgs.haskell.lib.dontCheck haskellPackages.hsparql;
    xsd = pkgs.haskell.lib.dontCheck haskellPackages.xsd;
    # esqueleto = haskellPackages.callHackage "esqueleto" "3.0.0" {};
  };

in
  pkgs.haskell.lib.overrideCabal cq2rdf (drv: {
    buildDepends = [ hdt pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram "$out/bin/cq2rdf-exe" \
        --prefix PATH ":" "${hdt}/bin"
    '';
  })
