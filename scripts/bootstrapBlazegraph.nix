{ pkgs }:

let

blazegraphJar = pkgs.fetchurl {
  url = "https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_RELEASE_2_1_5/blazegraph.jar";
  sha256 = "043nfc6mgmd5mxmwfcfl082y96iaqnwminn4rxbizxrs3dzaxbpv";
};

#in pkgs.writeScriptBin "bootstrapBlazegraph" ''
#  #!${pkgs.stdenv.shell}

#  PROPFILE=$TMP/RWStore.properties

#  cat <<EOT >> $PROPFILE
#quiet=false
#verbose=2
#namespace=kb
#com.bigdata.journal.AbstractJournal.file=blazegraph.jnl
##Files to load
#baseURI=http://data.cinematheque.qc.ca
#fileOrDirs=$1
#propertyFile=$PROPFILE
#EOT

#  ${pkgs.openjdk}/bin/java -server -Xmx4g -Djetty.port=9999 -jar ${blazegraphJar} &
#  BLAZEGRAPH_PID=$!

#  trap "kill $BLAZEGRAPH_PID ; rm -f $PROPFILE" EXIT

#  sleep 5
#  curl --data-binary @$PROPFILE --header 'Content-Type:text/plain' http://localhost:9999/blazegraph/dataloader
#''
in pkgs.runCommand "bootstrapBlazegraph" {
    buildInputs = with pkgs; [ makeWrapper curl openjdk ];
} ''
  mkdir -p $out/bin
  cp ${./bootstrapBlazegraph.sh} $out/bin/bootstrapBlazegraph
  wrapProgram "$out/bin/bootstrapBlazegraph" \
      --prefix PATH : $PATH --prefix BLAZEGRAPH_JAR : ${blazegraphJar}
''

