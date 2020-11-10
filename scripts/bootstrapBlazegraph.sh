#!/usr/bin/env sh

PROPFILE=$TMP/$$.properties

cat <<EOT >> $PROPFILE
quiet=false
verbose=2
namespace=kb
com.bigdata.journal.AbstractJournal.file=blazegraph.jnl
#Files to load
baseURI=http://data.cinematheque.qc.ca
fileOrDirs=$1
propertyFile=$PROPFILE
EOT

java -server -Xmx4g -Djetty.port=9999 -jar $BLAZEGRAPH_JAR &
BLAZEGRAPH_PID=$!

trap "kill $BLAZEGRAPH_PID ; rm -f $PROPFILE" EXIT

sleep 5
curl --data-binary @$PROPFILE --header 'Content-Type:text/plain' http://localhost:9999/blazegraph/dataloader
