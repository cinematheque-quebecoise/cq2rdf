-- This file is part of cq2rdf.

-- cq2rdf is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- cq2rdf is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with cq2rdf.  If not, see <https://www.gnu.org/licenses/>.

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
