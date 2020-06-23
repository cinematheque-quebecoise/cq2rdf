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

SOURCES := $(shell find . -name '*.hs')

build: $(EXEC)

$(EXEC): release.nix *.nix *.yaml $(SOURCES)
	nix-build $< && rm -f $(EXEC) && cp result/bin/$(EXEC) .

run: $(EXEC)
	./$(EXEC) -b $(BASEURI) -s $(CINETV_PUBLIC_SQLITE) -o $(DESTDIR)

# If release tag does not exist on Gitlab server, it returns a 403 Forbidden HTTP code.
# @param token - Private Gitlab token
# $ make release token=<YOURTOKEN>
release: $(DESTDIR)/cmtq-dataset
	./create-release.sh \
		"cq2rdf v$(VERSION)" \
		"v$(VERSION)" \
		$(GITLAB_PROJECT_ID) \
		"New release of cq2rdf v$(VERSION)" \
		$(token) \
		$<

clean:
	rm $(EXEC)
