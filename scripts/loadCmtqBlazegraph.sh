PROP_FILE=RWStore.properties

cat <<EOT >> $PROP_FILE
quiet=false
verbose=2
namespace=kb
#Files to load
baseURI=http://data.cinematheque.qc.ca
fileOrDirs=$1
# propertyFile=RWStore.properties
EOT

rm -f blazegraph.jnl
java -server -Xmx4g -Djetty.port=9999 -Dcom.bigdata.journal.AbstractJournal.file=blazegraph.jnl -jar blazegraph.jar &
sleep 5
curl --data-binary @${PROP_FILE} --header 'Content-Type:text/plain' http://localhost:9999/blazegraph/dataloader &
wait
