{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Namespaces where

import qualified Data.RDF as RDF
import qualified Data.RDF.Namespace as RDF

cmtq :: RDF.Namespace
cmtq = RDF.mkPrefixedNS' "cmtq" "http://localhost:8080/resource/"

cmtqo :: RDF.Namespace
cmtqo = RDF.mkPrefixedNS' "cmtqo" "http://data.cinematheque.qc.ca/ontology/cmtqo/0.1#"

crm :: RDF.Namespace
crm = RDF.mkPrefixedNS' "crm" "http://www.cidoc-crm.org/cidoc-crm/"

frbroo :: RDF.Namespace
frbroo = RDF.mkPrefixedNS' "frbroo" "http://iflastandards.info/ns/fr/frbr/frbroo/"

dbo :: RDF.Namespace
dbo = RDF.mkPrefixedNS' "dbo" "http://dbpedia.org/ontology/"

schema :: RDF.Namespace
schema = RDF.mkPrefixedNS' "schema" "http://schema.org/"

rdac :: RDF.Namespace
rdac = RDF.mkPrefixedNS' "rdac" "http://rdaregistry.info/Elements/c/"

dcterms :: RDF.Namespace
dcterms = RDF.mkPrefixedNS' "dcterms" "http://purl.org/dc/terms/"

prefixMappings :: RDF.PrefixMappings
prefixMappings = RDF.ns_mappings [ cmtq
                                 , cmtqo
                                 , crm
                                 , frbroo
                                 , dbo
                                 , schema
                                 , rdac
                                 , dcterms
                                 , RDF.rdf
                                 , RDF.rdfs
                                 , RDF.xsd
                                 , RDF.foaf
                                 ]
