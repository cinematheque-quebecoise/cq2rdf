# Date of data exportation.
DATE=2019-07-12
CINETV_PUBLIC_SQLITE=${HOME}/Documents/cinetv/cinetv-$(DATE)/cinetv-$(DATE)-publique.db
BASEURI=http://data.cinematheque.qc.ca
# Destination directory of produced files
DESTDIR=.
# Executable name
EXEC := $(shell grep "name:\s*" package.yaml | sed "s/name:\s*\(.*\)\s*/\1/")-exe
GITLAB_PROJECT_ID=18031890
VERSION := $(shell grep "version:\s*" package.yaml | sed "s/version:\s*\(.*\)\s*/\1/")
GITLAB_TOKEN := $(shell cat .gitlab-token)

CONVERSION_MODULES := $(shell find src/CineTV/RDF/Conversion -name "*.hs" -print)
DATA_RDF_MODULES := $(shell find src/Data/RDF -name "*.hs" -print)
VOCAB_HASKELL_MODULES := $(shell find src/SW -name "*.hs" -print)

build:
	cabal build

cinetv-to-rdf: $(DESTDIR)/cmtq-dataset/cmtq-dataset.nt.gz $(DESTDIR)/cmtq-dataset/cmtq-dataset.ttl.gz $(DESTDIR)/cmtq-dataset/cmtq-dataset.hdt

generate-void: $(DESTDIR)/cmtq-dataset/void.ttl

$(DESTDIR)/cmtq-dataset/cmtq-dataset.nt.gz: app/Main.hs src/Run.hs src/Run/CinetvToRdf.hs src/CineTV/RDF/Conversion.hs $(CONVERSION_MODULES) $(DATA_RDF_MODULES) $(VOCAB_HASKELL_MODULES)
	cabal run $(EXEC) -- cinetv-to-rdf -b $(BASEURI) -s $(CINETV_PUBLIC_SQLITE) -o $(DESTDIR)

$(DESTDIR)/cmtq-dataset/cmtq-dataset.ttl.gz: app/Main.hs src/Run.hs src/Run/CinetvToRdf.hs src/CineTV/RDF/Conversion.hs $(CONVERSION_MODULES) $(DATA_RDF_MODULES) $(VOCAB_HASKELL_MODULES)
	cabal run $(EXEC) -- cinetv-to-rdf -b $(BASEURI) -s $(CINETV_PUBLIC_SQLITE) -o $(DESTDIR)

$(DESTDIR)/cmtq-dataset/cmtq-dataset.hdt: app/Main.hs src/Run.hs src/Run/CinetvToRdf.hs src/CineTV/RDF/Conversion.hs $(CONVERSION_MODULES) $(DATA_RDF_MODULES) $(VOCAB_HASKELL_MODULES)
	cabal run $(EXEC) -- cinetv-to-rdf -b $(BASEURI) -s $(CINETV_PUBLIC_SQLITE) -o $(DESTDIR)

$(DESTDIR)/cmtq-dataset/void.ttl: app/Main.hs src/Run.hs src/Run/GenerateVoid.hs src/CineTV/RDF/Void.hs $(CONVERSION_MODULES) $(DATA_RDF_MODULES) $(VOCAB_HASKELL_MODULES) blazegraph.jnl
	generateVoidDataset $(CINETV_PUBLIC_SQLITE) $(DESTDIR)

# If release tag does not exist on Gitlab server, it returns a 403 Forbidden HTTP code.
# @param token - Private Gitlab token
# $ make release token=<YOURTOKEN>
release: $(DESTDIR)/cmtq-dataset $(DESTDIR)/example-queries.yaml .gitlab-token
	cp $(DESTDIR)/example-queries.yaml $(DESTDIR)/cmtq-dataset
	./scripts/create-release.sh \
		"cq2rdf v$(VERSION)" \
		"v$(VERSION)" \
		$(GITLAB_PROJECT_ID) \
		"New release of cq2rdf v$(VERSION)" \
		$(GITLAB_TOKEN) \
		$<

test-sparql-queries: blazegraph.jnl
	test_sparql_queries example-queries.yaml

blazegraph.jnl: cmtq-dataset/cmtq-dataset.ttl.gz
	rm blazegraph.jnl
	bootstrapBlazegraph cmtq-dataset/cmtq-dataset.ttl.gz

clean:
	rm $(EXEC) blazegraph.jnl
