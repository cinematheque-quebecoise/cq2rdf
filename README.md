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

### Développement

Pour développer, placez-vous dans un environnement isolé avec 

```
nix-shell --pure
```

La compilation se fait avec l'outil `Cabal` (devrait être disponible grace à `nix-shell --pure`)

```
cabal build
```

Exécutez le programme avec 

```
cabal run cq2rdf-exe -- --help
```

### Génération d'un exécutable

Générer l'exécutable avec :

```
$ nix-build release.nix
```

L'exécutable devient disponible dans le chemin relatif `./result/bin/cq2rdf-exe`.

## Publication des données

Assurez-vous de modifier correction la version du logiciel dans le fichier `package.yaml`. Ensuite, publiez les données avec:

```
$ make create-release
```

## License

cq2rdf est licensié sous la license [GPLv3](https://opensource.org/licenses/gpl-3.0.html). Voir le fichier [LICENSE](./LICENSE) pour obtenir le texte complet de la license.
