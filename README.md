# cq2rdf

## Execute

* Run `stack exec -- cq2rdf-exe` to see "We're inside the application!"
* With `stack exec -- cq2rdf-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test`

## Tâche de migration à faire

- Inverser le prénom + nom
- Ajouter la représentation d'un lieu de production d'un Film. Comment la CQ a encodé ça?
- Créer la relation de travail dérivé avec la fonction "Source Originale" d'un générique.
- Standardiser dates et intervals de temps (p. ex. E52_Time-Span) par TimeOntology.
- Modifier les noms de Place par une instance de Wikidata.
- Ajouter un lien vers Wikidata pour les films.
- Ajouter un lien vers Wikidata pour les personnes.
- Convertir les dates en datetime valide.

## Correction à apporter aux données

- Person145032 (https://www.imdb.com/name/nm0004813/) possède le nom "I Peed My Pants Nance", mais son vrai nom est Nancy Cartwright. Solution: Ajouter champ "Noms alternatifs" dans CineTV.
- L'entité avec NomID 23694 possède le nom "voir précision", mais il y a aucune précision. Environ 650 productions réfèrent à elle dans le générique.
- Le Sujet 19993 se trouve dans la table Filmo_GenresCategories pour le Filmo 102665, mais ce sujet ne se trouve pas dans la table Sujet.
- C'est normal que certains que la table Filmo_Realisation contient des identifiants de Filmo qui ne sont pas dans la table Filmo?
- Ajouter une fonction "Réalisation" dans la liste d'autorité Fonctions.
- Désambiguîser la liste d'autorité Pays vers ceux de Wikidata et modifier le code pour utiliser cette désambiguïsation.
- Désambiguîser la liste d'autorité Province vers ceux de Wikidata et modifier le code pour utiliser cette désambiguïsation.
- Désambiguîser la liste d'autorité Personne vers ceux de Wikidata et modifier le code pour utiliser cette désambiguïsation.
- Lier les films vers ceux de Wikidata et modifier le code pour utiliser cette liaison.
- Lier les fonctions vers ceux de Wikidata.
