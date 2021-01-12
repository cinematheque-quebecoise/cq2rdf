#!/usr/bin/env sh

PROPFILE=$TMP/$$.properties

cat <<EOT >> $PROPFILE
quiet=false
verbose=2
# namespace=kb
com.bigdata.journal.AbstractJournal.file=blazegraph.jnl
com.bigdata.rdf.store.AbstractTripleStore.quads=false
#Files to load
baseURI=http://data.cinematheque.qc.ca
fileOrDirs=$1
propertyFile=$PROPFILE
EOT

java -server -Xmx4g -Dcom.bigdata.rdf.sail.webapp.ConfigParams.propertyFile=$PROPFILE -Djetty.port=9999 -jar $BLAZEGRAPH_JAR
