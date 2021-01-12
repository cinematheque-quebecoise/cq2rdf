{ pkgs }:

let

blazegraphJar = pkgs.fetchurl {
  url = "https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_RELEASE_2_1_5/blazegraph.jar";
  sha256 = "043nfc6mgmd5mxmwfcfl082y96iaqnwminn4rxbizxrs3dzaxbpv";
};

in pkgs.runCommand "generateVoidDataset" {
    buildInputs = with pkgs; [ makeWrapper openjdk ];
} ''
  mkdir -p $out/bin
  cp ${./generateVoidDataset.sh} $out/bin/generateVoidDataset

  wrapProgram "$out/bin/generateVoidDataset" \
      --prefix PATH : $PATH --prefix BLAZEGRAPH_JAR : ${blazegraphJar}
''
