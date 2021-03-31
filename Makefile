# Date of data exportation.
BASEURI=http://data.cinematheque.qc.ca
# Destination directory of produced files
DESTDIR=cmtq-dataset
# Executable name
EXEC := $(shell grep "name:\s*" package.yaml | sed "s/name:\s*\(.*\)\s*/\1/")-exe
GITLAB_PROJECT_ID=18031890
VERSION := $(shell grep "version:\s*" package.yaml | sed "s/version:\s*\(.*\)\s*/\1/")
GITLAB_TOKEN := $(shell cat .gitlab-token)

CONVERSION_MODULES := $(shell find src/CineTV/RDF/Conversion -name "*.hs" -print)
DATA_RDF_MODULES := $(shell find src/Data/RDF -name "*.hs" -print)

CINETV_SQLITE_DB := $(shell basename $(CINETV_SQLITE_DB_PATH))

cinetv-to-rdf: $(DESTDIR)/cmtq-dataset.nt.gz $(DESTDIR)/cmtq-dataset.ttl.gz $(DESTDIR)/cmtq-dataset.hdt

generate-void: $(DESTDIR)/void.ttl

$(DESTDIR)/cmtq-dataset.nt.gz: $(DESTDIR)/cmtq-dataset.ttl.gz
$(DESTDIR)/cmtq-dataset.hdt: $(DESTDIR)/cmtq-dataset.ttl.gz
$(DESTDIR)/cmtq-dataset.ttl.gz: app/Main.hs src/Run.hs src/Run/CinetvToRdf.hs $(CINETV_SQLITE_DB) $(shell find src/Data/CQLOD/Readers -name "*.hs" -print) $(shell find src/Data/CQLOD/Writers -name "*.hs" -print) $(DATA_RDF_MODULES)
	cabal run $(EXEC) -- cinetv-to-rdf -b $(BASEURI) -s $(CINETV_SQLITE_DB) -o $(DESTDIR)

$(DESTDIR)/void.ttl: app/Main.hs src/Run.hs src/Run/GenerateVoid.hs src/Data/CQLOD/RDF/Void.hs $(DATA_RDF_MODULES) blazegraph.jnl
	generateVoidDataset $(CINETV_SQLITE_DB) $(DESTDIR)

# If release tag does not exist on Gitlab server, it returns a 403 Forbidden HTTP code.
# @param token - Private Gitlab token
# $ make release token=<YOURTOKEN>
release: $(DESTDIR) $(DESTDIR)/cmtq-dataset.ttl.gz $(DESTDIR)/cmtq-dataset-csv.tar.gz example-queries.yaml $(DESTDIR)/void.ttl .gitlab-token
	cp example-queries.yaml $(DESTDIR)
	./scripts/create-release.sh \
		"cq2rdf v$(VERSION)" \
		"v$(VERSION)" \
		$(GITLAB_PROJECT_ID) \
		"New release of cq2rdf v$(VERSION)" \
		$(GITLAB_TOKEN) \
		$<

$(DESTDIR)/cmtq-dataset-csv.tar.gz: ${CINETV_CSV_FILES_PATH}
	tar czf $(DESTDIR)/cmtq-dataset-csv.tar.gz -C ${CINETV_CSV_FILES_PATH} $(shell ls ${CINETV_CSV_FILES_PATH})

test-sparql-queries: blazegraph.jnl
	test_sparql_queries example-queries.yaml

blazegraph.jnl: $(DESTDIR)/cmtq-dataset.ttl.gz
	rm -f blazegraph.jnl
	bootstrapBlazegraph $(DESTDIR)/cmtq-dataset.ttl.gz

$(CINETV_SQLITE_DB): ${CINETV_SQLITE_DB_PATH}
	cp ${CINETV_SQLITE_DB_PATH} .

clean:
	rm -rf $(EXEC) blazegraph.jnl cmtq-dataset $(CINETV_SQLITE_DB)* dist-newstyle
