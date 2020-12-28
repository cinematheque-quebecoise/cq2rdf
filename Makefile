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

build: $(EXEC)

$(EXEC): release.nix
	nix-build $< && rm -f $(EXEC) && cp result/bin/$(EXEC) .

run: $(EXEC) $(CINETV_PUBLIC_SQLITE) $(DESTDIR)
	./$(EXEC) -b $(BASEURI) -s $(CINETV_PUBLIC_SQLITE) -o $(DESTDIR)

run-dev: $(CINETV_PUBLIC_SQLITE) $(DESTDIR)
	cabal run $(EXEC) -- -b $(BASEURI) -s $(CINETV_PUBLIC_SQLITE) -o $(DESTDIR)

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

bootstrap-blazegraph:
	bootstrapBlazegraph cmtq-dataset/cmtq-dataset.ttl.gz

test-sparql-queries:
	test_sparql_queries example-queries.yaml

clean:
	rm $(EXEC)