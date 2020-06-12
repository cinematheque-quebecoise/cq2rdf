{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Namespaces where

import Data.RDF
import Data.RDF.Namespace

cmtqo :: Namespace
cmtqo = mkPrefixedNS' "cmtqo" "http://data.cinematheque.qc.ca/ontology/cmtqo/0.1#"

crm :: Namespace
crm = mkPrefixedNS' "crm" "http://www.cidoc-crm.org/cidoc-crm/"

frbroo :: Namespace
frbroo = mkPrefixedNS' "frbroo" "http://iflastandards.info/ns/fr/frbr/frbroo/"

dbo :: Namespace
dbo = mkPrefixedNS' "dbo" "http://dbpedia.org/ontology/"

schema :: Namespace
schema = mkPrefixedNS' "schema" "http://schema.org/"

rdac :: Namespace
rdac = mkPrefixedNS' "rdac" "http://rdaregistry.info/Elements/c/"

dcterms :: Namespace
dcterms = mkPrefixedNS' "dcterms" "http://purl.org/dc/terms/"

wd :: Namespace
wd = mkPrefixedNS' "wd" "http://www.wikidata.org/entity/"

wdt :: Namespace
wdt = mkPrefixedNS' "wdt" "http://www.wikidata.org/prop/direct/"

prefixMappings :: PrefixMappings
prefixMappings = ns_mappings [ cmtqo
                             , crm
                             , frbroo
                             , dbo
                             , schema
                             , rdac
                             , dcterms
                             , rdf
                             , rdfs
                             , xsd
                             , foaf
                             , owl
                             , wd
                             , wdt
                             ]
