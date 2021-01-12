# cq2rdf

Programme qui convertit les données de la Cinémathèque québécoise en RDF.

## Prérequis

1. Le fichier SQLite qui représente la version publique de CineTV (ex. `cinetv-2019-07-12-publique.db`). Voir les « releases » du dépôt [cinetvdb](https://gitlab.com/cinematheque-quebecoise/cinetvdb).
2. Avoir préinstallé l'outil `cabal` ou [Nix](https://nixos.org/).

## Usage

```
$ cq2rdf-exe --help
cq2rdf Copyright (C) 2020 Cinemathèque québécoise This program comes with
ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to
redistribute it under certain conditions.

Usage: cq2rdf-exe [--version] [--help] [-v|--verbose] COMMAND
                  (-b|--baseuri BASEURI) (-s|--sqlitedb SQLITEDBFILE)
                  (-o|--outputdir OUTPUTDIR)
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

Available commands:
  cinetv-to-rdf            Convert CineTV to RDF
  generate-void            Generate a VoID dataset from SPARQL endpoint
```

## Environnement de développement avec Nix

Si [Nix](https://nixos.org/) est installé dans votre système, alors vous pouvez initialiser l'environnement de développement avec:

```
$ nix-shell
```

### Développement

Le programme peut être compilé avec:

```
$ cabal build
```

Il peut être exécuté avec:

```
cabal run cq2rdf-exe -- --help
```

Il existe dans le `Makefile` des commandes prédéfinies telles que:

```
$ make build
$ make cinetv-to-rdf
$ make generate-void
```

### Génération d'un exécutable avec Nix

Générer l'exécutable avec :

```
$ make build
```

L'exécutable devient disponible dans le chemin relatif `./result/bin/cq2rdf-exe`.


### Tests unitaires

Pour exécuter les tests unitaires, exécutez la commande `cabal test`.

### Tests de requêtes SPARQL

Plusieurs requêtes SPARQL sont définies dans le fichier `example-queries.yaml`. Afin de valider que ces requêtes SPARQL fonctionne sur les données RDF, nous utilisons la base de données Blazegraph.

Tout d'abord, exécutez la commande `make bootstrap-blazegraph` pour migrer les données RDF dans Blazegraph. Ensuite, exécutez la commande `make test-sparql-queries` pour tester les requêtes SPARQL.

## Publication des données

Assurez-vous de modifier la version du logiciel dans le fichier `package.yaml`.

Pour publier une nouvelle version des données, vous devez générer un jeton à partir de l'interface web de Gitlab. Ce jeton est appellé un « [Personal Access Token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) ».
Mettez ce jeton dans le fichier caché `.gitlab-token` à la racine du projet.

La commande suivante va téléverser le dossier qui a été généré par la commande `make run` ou `make run-dev`:

```
$ make release
```

## License

cq2rdf est licensié sous la license [GPLv3](https://opensource.org/licenses/gpl-3.0.html). Voir le fichier [LICENSE](./LICENSE) pour obtenir le texte complet de la license.
