{ pkgs }:

let

blazegraphJar = pkgs.fetchurl {
  url = "https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_RELEASE_2_1_5/blazegraph.jar";
  sha256 = "043nfc6mgmd5mxmwfcfl082y96iaqnwminn4rxbizxrs3dzaxbpv";
};

in pkgs.runCommand "startBlazegraph" {
    buildInputs = with pkgs; [ makeWrapper curl openjdk ];
} ''
  mkdir -p $out/bin
  cp ${./startBlazegraph.sh} $out/bin/startBlazegraph
  wrapProgram "$out/bin/startBlazegraph" \
      --prefix PATH : $PATH --prefix BLAZEGRAPH_JAR : ${blazegraphJar}
''


