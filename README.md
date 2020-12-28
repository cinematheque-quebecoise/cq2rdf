# cq2rdf

A tool to convert the database of Cinemathèque québécoise in multple RDF formats.

*Read this in other languages: [English](README.md), [Français](README.fr.md).*

## Prerequisites

1. SQLite database file containing Cinemathèque québécoise's data (ex. `cinetv-2019-07-12-publique.db`). See releases from repository [cinetvdb](https://gitlab.com/cinematheque-quebecoise/cinetvdb).
2. `cabal` or [Nix](https://nixos.org/) must be installed.

## Usage

```
$ cq2rdf-exe --help
cq2rdf Copyright (C) 2020 Cinemathèque québécoise This program comes with
ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to
redistribute it under certain conditions.

Usage: cq2rdf-exe [--version] [--help] [-v|--verbose] (-b|--baseuri BASEURI)
                  (-s|--sqlitedb SQLITEDBFILE) (-o|--outputdir OUTPUTDIR)
  cq2rdf is a tool to convert the database of Cinemathèque québécoise in multple
  RDF formats.

Available options:
  --version                Show version
  --help                   Show this help text
  -v,--verbose             Verbose output?
  -b,--baseuri BASEURI     RDF Base URI
  -s,--sqlitedb SQLITEDBFILE
                           File path of the Sqlite database file
  -o,--outputdir OUTPUTDIR Output directory of result
```

## Environnement de développement avec Nix

With [Nix](https://nixos.org/) installed, go inside a dev environment with:

```
$ nix-shell
```

### Development

To compile the program:

```
$ cabal build
```

It can be executed with:

```
$ cabal run cq2rdf-exe -- --help
```

Predefined commands are available in the `Makefile` such as:

```
$ make run-dev
```

### Generating executable with Nix

Generate executable with:

```
$ make build
```

Which will be available in `./result/bin/cq2rdf-exe`.

To run the generated executable:

```
$ make run
```

### Unit tests

Unit tests can be launched with `cabal test`.

### SPARQL queries tests

Predefined SPARQL queries are available in the file `example-queries.yaml`. They are used to valide the correctness of the generated RDF files. Blazegraph is used to test those queries.

First off, execute `make bootstrap-blazegraph` to import the RDF data inside Blazegraph. Afterwards, execute `make test-sparql-queries` to test the SPARQL queries.

### Dataset description with VoID

The generated data can be described using the VoID ontology.

Some default information on the RDF dataset is defined in the file `void.base.ttl`. These values can be changed if applicable.

Other statements, such as number of triples with `void:triples`, are automatically inserted in the resulting file `void.ttl`.

## Data publication

Be sure to upgrade the version inside `package.yaml`.

Before publishing the RDF data, you need to generate a token from Gitlab's web interface. This token is called a [Personal Access Token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html). Put the new token in the hidden file `.gitlab-token`.

To publish the data the generated data, simply run:

```
$ make release
```

## License

cq2rdf is licenced in [GPLv3](https://opensource.org/licenses/gpl-3.0.html). See [LICENSE](./LICENSE) for a complete copy of the LICENSE.
