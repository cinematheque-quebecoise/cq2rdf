#!/usr/bin/env sh

PROPFILE=$TMP/$$.properties

cat <<EOT >> $PROPFILE
quiet=false
verbose=2
# False prevents renaming loaded datasets with extension .good or .fail
durableQueues=false
# namespace=kb
com.bigdata.journal.AbstractJournal.file=blazegraph.jnl
com.bigdata.rdf.store.AbstractTripleStore.quads=false
com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms
#Files to load
baseURI=http://data.cinematheque.qc.ca
fileOrDirs=$1
propertyFile=$PROPFILE
EOT

java -server -Xmx4g -Dcom.bigdata.rdf.sail.webapp.ConfigParams.propertyFile=$PROPFILE -Djetty.port=9999 -jar $BLAZEGRAPH_JAR &

BLAZEGRAPH_PID=$!

trap "kill $BLAZEGRAPH_PID ; rm -f $PROPFILE" EXIT

sleep 5

curl --data-binary @$PROPFILE --header 'Content-Type:text/plain' http://localhost:9999/blazegraph/dataloader
