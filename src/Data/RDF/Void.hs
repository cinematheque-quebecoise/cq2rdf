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

module Data.RDF.Void where

import           RIO

data DatasetStats = DatasetStats
    { numTriples          :: Int
    , numEntities         :: Int
    , numClasses          :: Int
    , numProperties       :: Int
    , numDistinctSubjects :: Int
    , numDistinctObjects  :: Int
    , classPartitions     :: [ClassPartition]
    , propPartitions      :: [PropertyPartition]
    -- , linksets :: [Linkset]
    }

data ClassPartition = ClassPartition
    { classPartitionName        :: Text
    , classPartitionNumEntities :: Int
    }

data PropertyPartition = PropertyPartition
    { propertyPartitionName        :: Text
    , propertyPartitionNumEntities :: Int
    }

data Linkset = Linkset
    { linksetExternalDomain :: Text
    , linksetPredicate      :: Text
    , linsetNumTriples      :: Int
    }

-- RdfGraph to VoIDData
-- VoIDData to RdfGraph
-- Read void.base.ttl to RdfGraph
-- Modify fields

-- createDatasetStats :: RDF rdfImpl -> DatasetStats
--
-- addDatasetVoidData :: CreationDate
--                    -> IssuedDate
--                    -> PrefixMappings
--                    -> RDF rdfImpl
--                    -> RdfState rdfImpl m ()
-- addCreationDate :: Date -> RdfState rdfImpl m ()
-- addIssuedDate :: Date -> RdfState rdfImpl m ()
-- addPrefixesAsVocab :: PrefixMappings -> RdfState rdfImpl m ()
-- addNumOfTriples :: RDF rdfImpl -> RdfState rdfImpl m ()
-- addNumOfEntities :: RDF rdfImpl -> RdfState rdfImpl m ()
-- addNumOfClasses :: RDF rdfImpl -> RdfState rdfImpl m ()
-- addNumOfProperties :: RDF rdfImpl -> RdfState rdfImpl m ()
-- addNumOfDistinctSubjects :: RDF rdfImpl -> RdfState rdfImpl m ()
-- addNumOfDistinctObjects :: RDF rdfImpl -> RdfState rdfImpl m ()
-- addClassPartitions :: RDF rdfImpl -> RdfState rdfImpl m ()
-- addPropertyPartitions :: RDF rdfImpl -> RdfState rdfImpl m ()
-- addLinksetWikidata :: RDF rdfImpl -> RdfState rdfImpl m ()
