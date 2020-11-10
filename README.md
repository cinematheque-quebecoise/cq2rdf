# cq2rdf

Programme qui convertit les données de la Cinémathèque québécoise en RDF.

**Important:** Pour fonctionner, ce programme a besoin de sa base de données relationnelle au format SQLite qui n'est pas actuellement ouverte au publique.

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

## Compilation

Pour la compilation, l'outil [Nix](https://nixos.org/) doit être installé sur votre système.

À partir de la racine de ce projet, lancer un environnement nix:

```
nix-shell --pure
```

### Développement

Afin d'obtenir un cycle de compilation rapide pour le développement, nous
utilisons l'outil `Cabal` (devrait être disponible grace à `nix-shell --pure`)

```
cabal build
```

Testez le programme avec

```
cabal run cq2rdf-exe -- --help
```

### Génération d'un exécutable

Générer l'exécutable avec :

```
$ make build
```

L'exécutable devient disponible dans le chemin relatif `./result/bin/cq2rdf-exe`.

### Tests unitaires

Pour exécuter les tests unitaires, exécutez la commande `cabal test`.

### Tests de requêtes SPARQL

Plusieurs requêtes SPARQL sont définies dans le fichier `example-queries.yaml`. Afin de valider que ces requêtes SPARQL fonctionne sur les données RDF, nous utilisons la base de données Blazegraph.

Tout d'abord, exécutez la commande `make bootstrap-blazegraph` pour peupler Blazegraph avec les données RDF. Ensuite, exécutez la commande `make test-sparql-queries` pour tester les requêtes SPARQL.

## Publication des données

Assurez-vous de modifier correction la version du logiciel dans le fichier `package.yaml`.

Pour publier une nouvelle version des données, vous devez générer un jeton à partir de l'interface web de Gitlab. Ce jeton est appellé un « [Personal Access Token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) ».
Mettez ce jeton dans le fichier caché `.gitlab-token` à la racine du projet.

Cette publication va téléverser le dossier qui a été généré par la commande `make run`.

Pour publier les données:

```
$ make release
```

## License

cq2rdf est licensié sous la license [GPLv3](https://opensource.org/licenses/gpl-3.0.html). Voir le fichier [LICENSE](./LICENSE) pour obtenir le texte complet de la license.
