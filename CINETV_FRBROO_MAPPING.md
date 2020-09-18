# Mapping de CineTV vers CIDOC CRM/FRBRoo

Ce document décrit la procédure de *mapping* de la base de données SQL **CineTV** vers l'ontology **FRBRoo** (dérivée de **CIDOC CRM**).

Le document est consistué d'un ensemble de sections où chaque section présente comment convertir une table SQL de CineTV en un ensemble de triplet RDF en utilisant le vocabulaire de **FRBRoo**.

**FRBRoo** est une ontologie formelle conçue par deux groupes: *CIDOC Conceputal Reference Model* et *Functional Requirements for Bibliographic Records*. L'objectif est de représenter sémantiquement les informations bibliographique et de faciliter l'integregation et l'échange d'information entre les institutions.

Nous utiliserons le format **RDF** pour encoder les données de **CineTV** vers **FRBRoo**. **RDF** est un modèle de graphe pour représenter par un ensemble d'énoncés/faits. Un énoncé est un triple Sujet-Prédicat-Object. Par exemple, le triplet suivant déclare une oeuvre:

```
http://data.cinematheque.qc.ca/resource/Work105 http://www.w3.org/1999/02/22-rdf-syntax-ns#type http://iflastandards.info/ns/fr/frbr/frbroo/F1_Work
```

Avec RDF, chaque concept est représenté par une URI. Dans le triplet précédent, le sujet est http://data.cinematheque.qc.ca/resource/Work105, le prédicat http://www.w3.org/1999/02/22-rdf-syntax-ns#type et l'objet http://iflastandards.info/ns/fr/frbr/frbroo/F1_Work.

Pour raccourcir l'écriture des triplets, nous allons définir des espaces de nommages avec la syntaxe Turtle. Par exemple:

```
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix frbroo: <http://iflastandards.info/ns/fr/frbr/frbroo/>
@prefix cmtq: <http://data.cinematheque.qc.ca/resource/>
```

Ces espaces de nommages nous permettent de réécrire le triplet précédent par :

```
cmtq:Work105 rdf:type frbroo:F1_Work .
```

Voici la liste des espaces de nommages qui seront utilisés dans le reste du document lorsqu'on écrira des triples :

```
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix crm: <http://www.cidoc-crm.org/cidoc-crm/>
@prefix frbroo: <http://iflastandards.info/ns/fr/frbr/frbroo/>
@prefix cmtq: <http://data.cinematheque.qc.ca/resource/>
```

## Table Langue

La table *Langue* contient deux colonnes: l'identifiant de la langue et le terme français de la langue. Ex: 

| LangueId | terme |
| :-: | :-: | 
| 8 | anglais   |
| 38   | français |

Chaque ligne de la table sera convertie en triplets. Voici les triplets pour représenter la première ligne:

```
cmtq:Language8 rdf:type crm:E56_Language
cmtq:Language8 rdfs:label "anglais"@fr
cmtq:Language8 crm:P1_is_identified_by cmtq:AppellationLanguage8
cmtq:Language8 crm:P48_has_preferred_identifier cmtq:IdentifierLanguage8
cmtq:AppellationLanguage8 crm:P190 "anglais"
cmtq:IdentifierLanguage8 crm:P190 "8"
```

Tout d'abord, on indique que le concept `cmtq:Language8` est une instance (`rdf:type`) de la classe `crm:E56_Language` de CIDOC CRM. Ensuite, on indique l'appellation du concept par la chaîne de caractères "anglais" et son identifiant par la chaîne de caractères "8".