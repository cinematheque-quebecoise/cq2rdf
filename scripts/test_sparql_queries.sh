#!/usr/bin/env sh

PROP_FILE=/tmp/$$.properties

cat <<EOT >> $PROP_FILE
quiet=false
verbose=2
namespace=kb
com.bigdata.journal.AbstractJournal.file=blazegraph.jnl
EOT

WEBXMLFILE=/tmp/$$.xml

cat <<EOT >> $WEBXMLFILE
<?xml version="1.0" encoding="UTF-8"?>
<web-app xmlns="http://java.sun.com/xml/ns/javaee"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_3_1.xsd"
      version="3.1">
  <context-param>
   <description>When true, the REST API will not permit mutation operations.</description>
   <param-name>readOnly</param-name>
   <param-value>true</param-value>
  </context-param>
</web-app>
EOT

java -server -Xmx4g -Djetty.port=9999 -Djetty.overrideWebXml=$WEBXMLFILE -Dcom.bigdata.rdf.sail.webapp.ConfigParams.propertyFile=$PROP_FILE -jar $BLAZEGRAPH_JAR &
BLAZEGRAPH_PID=$!

trap "kill $BLAZEGRAPH_PID ; rm -f PROP_FILE ; rm -f WEBXMLFILE" EXIT

sleep 3

python scripts/test_sparql_queries.py -e http://localhost:9999/sparql -q $1
