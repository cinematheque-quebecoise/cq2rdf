{ mkDerivation, aeson, base, conduit, directory, esqueleto, hpack
, hspec, mtl, optparse-simple, persistent, persistent-sqlite
, persistent-template, rdf4h, resource-pool, rio, stdenv, text
, time
}:
mkDerivation {
  pname = "cq2rdf";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base conduit directory esqueleto mtl persistent
    persistent-sqlite persistent-template rdf4h resource-pool rio text
    time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base conduit directory esqueleto mtl optparse-simple
    persistent persistent-sqlite persistent-template rdf4h
    resource-pool rio text time
  ];
  testHaskellDepends = [
    aeson base conduit directory esqueleto hspec mtl persistent
    persistent-sqlite persistent-template rdf4h resource-pool rio text
    time
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/cq2rdf#readme";
  license = stdenv.lib.licenses.bsd3;
}
