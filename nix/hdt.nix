# Temporary package until nixpkgs accept the pull-request containing this
# package description.
{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "hdt-${version}";
  version = "1.3.3";

  src = fetchGit {
    name = "hdt-1.3.3";
    url = "https://github.com/rdfhdt/hdt-cpp.git";
    ref = "refs/tags/v${version}";
    rev = "b90d8a3cbb9d976c4a654d25762ee5063ff32a76";
  };

  phases = "unpackPhase patchPhase preConfigurePhases configurePhase buildPhase installPhase";

  buildInputs = [ pkgs.automake pkgs.autoconf pkgs.libtool pkgs.zlib pkgs.pkg-config pkgs.serd ];

  propogatedBuildInputs = [];

  patchPhase = "patchShebangs ./autogen.sh";

  preConfigurePhases = ''
    ./autogen.sh
  '';
}
