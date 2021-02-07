
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
module Data.RDF.Vocabulary.Void where

import           Import             hiding (void, (^.))

import           Data.RDF.Namespace (Namespace)
import qualified Data.RDF.Namespace as RDF

void :: Namespace
void = RDF.mkPrefixedNS' "void" "http://rdfs.org/ns/void#"

voidDatasetDescription :: Text
voidDatasetDescription = RDF.mkUri void "DatasetDescription"

voidDataset :: Text
voidDataset = RDF.mkUri void "Dataset"

voidFeature :: Text
voidFeature = RDF.mkUri void "feature"

voidSparqlEndpoint :: Text
voidSparqlEndpoint = RDF.mkUri void "sparqlEndpoint"

voidDataDump :: Text
voidDataDump = RDF.mkUri void "dataDump"

voidExampleResource :: Text
voidExampleResource = RDF.mkUri void "exampleResource"

voidUriSpace :: Text
voidUriSpace = RDF.mkUri void "uriSpace"

voidVocabulary :: Text
voidVocabulary = RDF.mkUri void "vocabulary"

voidTriples :: Text
voidTriples = RDF.mkUri void "triples"

voidEntities :: Text
voidEntities = RDF.mkUri void "entities"

voidClasses :: Text
voidClasses = RDF.mkUri void "classes"

voidProperties :: Text
voidProperties = RDF.mkUri void "properties"

voidDistinctSubjects :: Text
voidDistinctSubjects = RDF.mkUri void "distinctSubjects"

voidDistinctObjects :: Text
voidDistinctObjects = RDF.mkUri void "distinctObjects"

voidClassPartition :: Text
voidClassPartition = RDF.mkUri void "classPartition"

voidPropertyPartition :: Text
voidPropertyPartition = RDF.mkUri void "propertyPartition"

voidClass :: Text
voidClass = RDF.mkUri void "class"

voidProperty :: Text
voidProperty = RDF.mkUri void "property"

voidLinkset :: Text
voidLinkset = RDF.mkUri void "Linkset"

voidTarget :: Text
voidTarget = RDF.mkUri void "target"

voidSubset :: Text
voidSubset = RDF.mkUri void "subset"

voidLinkPredicate :: Text
voidLinkPredicate = RDF.mkUri void "linkPredicate"

