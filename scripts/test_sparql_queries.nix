{ pkgs }:

let

blazegraphJar = pkgs.fetchurl {
  url = "https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_RELEASE_2_1_5/blazegraph.jar";
  sha256 = "043nfc6mgmd5mxmwfcfl082y96iaqnwminn4rxbizxrs3dzaxbpv";
};

in pkgs.runCommand "test_sparql_queries" {
    buildInputs = with pkgs; [ makeWrapper openjdk python38 ];
} ''
  mkdir -p $out/bin
  cp ${./test_sparql_queries.sh} $out/bin/test_sparql_queries

  wrapProgram "$out/bin/test_sparql_queries" \
      --prefix PATH : $PATH --prefix BLAZEGRAPH_JAR : ${blazegraphJar}
''

