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
module Data.RDF.Vocabulary.DCTerms where

import           Import             hiding ((^.))

import qualified Data.RDF.Namespace as RDF

dcterms :: RDF.Namespace
dcterms = RDF.mkPrefixedNS' "dcterms" "http://purl.org/dc/terms/"

dctermsTitle :: Text
dctermsTitle = RDF.mkUri dcterms "title"

dctermsCreator :: Text
dctermsCreator = RDF.mkUri dcterms "creator"

dctermsPublisher :: Text
dctermsPublisher = RDF.mkUri dcterms "publisher"

dctermsCreated :: Text
dctermsCreated = RDF.mkUri dcterms "created"

dctermsIssued :: Text
dctermsIssued = RDF.mkUri dcterms "issued"

dctermsModified :: Text
dctermsModified = RDF.mkUri dcterms "modified"

dctermsLicense :: Text
dctermsLicense = RDF.mkUri dcterms "license"

dctermsSubject :: Text
dctermsSubject = RDF.mkUri dcterms "subject"

