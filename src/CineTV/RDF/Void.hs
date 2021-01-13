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

module CineTV.RDF.Void where

import           Data.RDF.State          (RdfState, addPrefixMappings,
                                          addTriple)
import           Data.RDF.Types.Extended (mkTriple, mkTripleLit)
import           Import                  hiding (void)
import           SW.Vocabulary
import           Util                    (utcTimeToText)

import           Control.Monad.State     (execStateT)
import           Data.Hashable           (Hashable (..))
import           Data.RDF                (LValue (..), Node (..), RDF, Rdf,
                                          Triple (..))
import qualified Data.RDF                as RDF
import           Data.RDF.Namespace      (foaf, mkUri, ns_mappings, owl, rdf,
                                          rdfs, xsd)
import qualified Data.Text               as T
import qualified Data.Text.Read               as T
import           Data.Time.Clock         (UTCTime (..))
import Database.HSparql.Connection (EndPoint, BindingValue(..))
import qualified Database.HSparql.Connection as HSparql
import Database.HSparql.QueryGenerator (Query, SelectQuery, SelectExpr(..), var, triple_, count, select, selectVars, as, union_, distinct_, subQuery_, iriRef, groupBy_, filterExpr_, contains, str)

data CinetvRdf = CinetvRdf
  { cinetvSnapshotTime :: UTCTime
  , cinetvIssuedTime   :: UTCTime
  , cinetvRdfSparqlEndpoint :: EndPoint
  }

class (Monad m) => MonadSparqlQuery m where
  selectQuery :: EndPoint -> Query SelectQuery -> m (Maybe [[BindingValue]])

instance MonadSparqlQuery IO where
  selectQuery = HSparql.selectQuery

datasetDescUri :: Text
datasetDescUri = "/void/Dataset"

datasetUri :: Text
datasetUri = datasetDescUri <> "/CinemathequeQuebecoiseLinkedOpenDatabase"

wikidataDatasetUri :: Text
wikidataDatasetUri = datasetDescUri <> "/Wikidata"

wikidataLinksetUri :: Text
wikidataLinksetUri =
  datasetDescUri <> "/Wikidata_CinemathequeQuebecoiseLinkedOpenDatabase"

createVoidGraph :: (MonadSparqlQuery m, Rdf a) => CinetvRdf -> m (RDF a)
createVoidGraph cinetvRdf = do
  execStateT
    (  defaultVoID
    >> addCreatedDateTime (cinetvSnapshotTime cinetvRdf)
    >> addIssuedDateTime (cinetvIssuedTime cinetvRdf)
    >> addVoidStats (cinetvRdfSparqlEndpoint cinetvRdf)
    >> addVoidPrefixMappings
    )
    RDF.empty

-- |Default information to be added to the VoID data file.
defaultVoID :: (Rdf a, Monad m) => RdfState a m ()
defaultVoID = do
  mapM_ addTriple $ mkTriple datasetDescUri rdfType voidDatasetDescription
  mapM_ addTriple $ mkTripleLit
    datasetDescUri
    dctermsTitle
    (PlainLL
      "A VoID Description of the Cinémathèque québécoise Linked Open Dataset"
      "en"
    )

  -- Created by Cinémathèque québécoise
  mapM_ addTriple $ mkTriple datasetDescUri dctermsCreator (mkUri wd "Q2973248")
  mapM_ addTriple $ mkTriple datasetDescUri foafPrimaryTopic datasetUri

  mapM_ addTriple $ mkTriple datasetUri rdfType voidDataset
  mapM_ addTriple $ mkTriple datasetUri
                             dctermsLicense
                             "http://www.opendatacommons.org/licenses/odbl/"

-- |Add time when the dataset was exported from CineTV.
addCreatedDateTime :: (Rdf a, Monad m) => UTCTime -> RdfState a m ()
addCreatedDateTime time = do
  let timeText = utcTimeToText time
  mapM_ addTriple
    $ mkTripleLit datasetUri dctermsCreated (TypedL timeText xsdDateTime)

-- |Add time when the dataset was generated in RDF.
addIssuedDateTime :: (Rdf a, Monad m) => UTCTime -> RdfState a m ()
addIssuedDateTime time = do
  let timeText = utcTimeToText time
  mapM_ addTriple
    $ mkTripleLit datasetUri dctermsIssued (TypedL timeText xsdDateTime)

-- |Add different statistics extracted from the dataset's SPARQL endpoint.
addVoidStats
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addVoidStats sparqlEndpoint = do
  addNumTriples sparqlEndpoint
  addNumEntities sparqlEndpoint
  addNumClasses sparqlEndpoint
  addNumProperties sparqlEndpoint
  addNumDistinctSubjects sparqlEndpoint
  addNumDistinctObjects sparqlEndpoint
  addClassPartitions sparqlEndpoint
  addPropertyPartitions sparqlEndpoint
  addWikidataLinkset sparqlEndpoint

-- |Add the void:triples triple in the VoID dataset.
addNumTriples
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ Sparql endpoint
  -> RdfState a m ()
addNumTriples sparqlEndpoint = do
  numTriples <- lift $ countTriples sparqlEndpoint -- length $ RDF.query graph Nothing Nothing Nothing
  mapM_ addTriple $ mkTripleLit datasetUri
                                voidTriples
                                (TypedL (T.pack $ show numTriples) xsdInteger)

-- Query the SPARQL endpoint that counts the total number of triples.
--
-- @
-- select (count(distinct ?s) as ?count) where {
--   ?s ?p ?o
-- }
-- @
countTriples :: (MonadSparqlQuery m)
             => EndPoint -- ^ SPARQL endpoint
             -> m Integer
countTriples sparqlEndpoint = do
  getCountQueryResult sparqlEndpoint countTriplesQuery

 where
  countTriplesQuery :: Query SelectQuery
  countTriplesQuery = do
    s <- var
    p <- var
    o <- var

    triple_ s p o

    countVar <- var

    select [count s `as` countVar]

-- |Add the void:entities triple in the VoID dataset.
addNumEntities
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addNumEntities sparqlEndpoint = do
  numEntities <- lift $ countNumEntities sparqlEndpoint
  mapM_ addTriple $ mkTripleLit
    datasetUri
    voidEntities
    (TypedL (T.pack $ show numEntities) xsdInteger)

-- Query the SPARQL endpoint that counts the total number of entities.
--
-- The number of entities correspond to the number of distinct subjects and
-- objects from triples.
--
-- @
-- select (count(?r) as ?count) where {
--   select distinct ?r where {
--     { ?r ?p ?o } UNION { ?s ?p ?r }
--     FILTER (contains(str(?r), "resource"))
--   }
-- }
-- @
countNumEntities :: (MonadSparqlQuery m)
                 => EndPoint -- ^ Sparql endpoint
                 -> m Integer
countNumEntities sparqlEndpoint = do
  getCountQueryResult sparqlEndpoint countEntitiesQuery

 where
  countEntitiesQuery :: Query SelectQuery
  countEntitiesQuery = do
    r <- var
    s <- var
    p <- var
    o <- var

    subQuery_ $ do
      let tt1 = triple_ r p o
          tt2 = triple_ s p r
       in do
         distinct_
         union_ tt1 tt2
         selectVars [r]

    countVar <- var
    select [count r `as` countVar]

-- |Add the void:classes triple in the VoID dataset.
addNumClasses
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addNumClasses sparqlEndpoint = do
  numClasses <- lift $ countNumClasses sparqlEndpoint
  mapM_ addTriple $ mkTripleLit datasetUri
                                voidClasses
                                (TypedL (T.pack $ show numClasses) xsdInteger)

-- Query the SPARQL endpoint that counts the total number of distinct classes.
--
-- The number of classes include all classes used in the dataset, even from
-- external databases.
--
-- @
-- select (count(?c) as ?count) where {
--   select distinct ?c where {
--     [] a ?c
--   }
-- }
-- @
countNumClasses :: (MonadSparqlQuery m)
                => EndPoint -- ^ Sparql endpoint
                -> m Integer
countNumClasses sparqlEndpoint = do
  getCountQueryResult sparqlEndpoint countClassesQuery

 where
   countClassesQuery :: Query SelectQuery
   countClassesQuery = do
      s <- var
      c <- var

      subQuery_ $ do
        triple_ s (iriRef rdfType) c
        distinct_
        selectVars [c]

      countVar <- var
      select [count c `as` countVar]

-- |Add the void:properties triple in the VoID dataset.
addNumProperties
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addNumProperties sparqlEndpoint = do
  numProperties <- lift $ countNumProperties sparqlEndpoint
  mapM_ addTriple $ mkTripleLit
    datasetUri
    voidProperties
    (TypedL (T.pack $ show numProperties) xsdInteger)

-- Query the SPARQL endpoint that counts the total number of distinct properties.
--
-- The number of properties include all properties used in the dataset, even
-- from external databases.
--
-- @
-- select (count(?p) as ?count) where {
--   select distinct ?p where {
--     [] p []
--   }
-- }
-- @
countNumProperties :: (MonadSparqlQuery m)
                   => EndPoint -- ^ Sparql endpoint
                   -> m Integer
countNumProperties sparqlEndpoint = do
  getCountQueryResult sparqlEndpoint countPropertiesQuery

 where
   countPropertiesQuery :: Query SelectQuery
   countPropertiesQuery = do
      s <- var
      p <- var
      o <- var

      subQuery_ $ do
        triple_ s p o
        distinct_
        selectVars [p]

      countVar <- var
      select [count p `as` countVar]

-- |Add the void:distinctSubjects triple in the VoID dataset.
addNumDistinctSubjects
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addNumDistinctSubjects sparqlEndpoint = do
  numDistinctSubjects <- lift $ countNumDistinctSubjs sparqlEndpoint
  mapM_ addTriple $ mkTripleLit
    datasetUri
    voidDistinctSubjects
    (TypedL (T.pack $ show numDistinctSubjects) xsdInteger)

-- Query the SPARQL endpoint that counts the total number of distinct subjects
-- of triples.
--
-- @
-- SELECT (COUNT(?s) as ?count) WHERE {
--   SELECT DISTINCT ?s WHERE {
--     ?s ?p ?o .
--   }
-- }
-- @
countNumDistinctSubjs :: (MonadSparqlQuery m)
                   => EndPoint -- ^ Sparql endpoint
                   -> m Integer
countNumDistinctSubjs sparqlEndpoint = do
  getCountQueryResult sparqlEndpoint countDistinctSubjsQuery

 where
   countDistinctSubjsQuery :: Query SelectQuery
   countDistinctSubjsQuery = do
      s <- var
      p <- var
      o <- var

      subQuery_ $ do
        triple_ s p o
        distinct_
        selectVars [s]

      countVar <- var
      select [count s `as` countVar]

-- |Add the void:distinctObjects triple in the VoID dataset.
addNumDistinctObjects
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addNumDistinctObjects sparqlEndpoint = do
  numDistinctObjects <- lift $ countNumDistinctObjs sparqlEndpoint
  mapM_ addTriple $ mkTripleLit
    datasetUri
    voidDistinctObjects
    (TypedL (T.pack $ show numDistinctObjects) xsdInteger)

-- Query the SPARQL endpoint that counts the total number of distinct objects
-- of triples.
--
-- @
-- SELECT (COUNT(?o) as ?count) WHERE {
--   SELECT DISTINCT ?o WHERE {
--     ?s ?p ?o .
--   }
-- }
-- @
countNumDistinctObjs :: (MonadSparqlQuery m)
                     => EndPoint -- ^ Sparql endpoint
                     -> m Integer
countNumDistinctObjs sparqlEndpoint = do
  getCountQueryResult sparqlEndpoint countDistinctObjsQuery

 where
   countDistinctObjsQuery :: Query SelectQuery
   countDistinctObjsQuery = do
      s <- var
      p <- var
      o <- var

      subQuery_ $ do
        triple_ s p o
        distinct_
        selectVars [o]

      countVar <- var
      select [count o `as` countVar]

-- |Add all void:classPartition triples in the VoID dataset.
addClassPartitions
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addClassPartitions sparqlEndpoint = do
  classPartitions <- lift $ selectClassPartitions sparqlEndpoint
  forM_ classPartitions addClassPartition

 where
   addClassPartition :: (Rdf a, Monad m) => (Text, Integer) -> RdfState a m ()
   addClassPartition (classUri, numEntities) = do
     let classPartitionNode = RDF.unode $ datasetUri <> "/ClassPartition" <> T.pack (show $ abs $ hash classUri)
     addTriple $ Triple (RDF.unode datasetUri)
                        (RDF.unode voidClassPartition)
                        classPartitionNode
     addTriple
       $ Triple classPartitionNode (RDF.unode voidClass) (RDF.unode classUri)
     addTriple $ Triple
       classPartitionNode
       (RDF.unode voidEntities)
       (RDF.lnode $ TypedL (T.pack $ show numEntities) xsdInteger)

-- Query the SPARQL endpoint for the number of entities per class.
--
-- @
-- SELECT ?o (COUNT(?s) as ?count) WHERE {
--   ?s a ?o .
-- } GROUPBY ?o
-- @
selectClassPartitions :: (MonadSparqlQuery m)
                      => EndPoint -- ^ Sparql endpoint
                      -> m [(Text, Integer)]
selectClassPartitions sparqlEndpoint = do
  getGroupByCountQueryResult sparqlEndpoint selectClassPartitionsQuery

 where
   selectClassPartitionsQuery :: Query SelectQuery
   selectClassPartitionsQuery = do
      s <- var
      o <- var

      triple_ s (iriRef rdfType) o

      groupBy_ o

      countVar <- var
      select [SelectVar o, count s `as` countVar]

-- |Add all void:propertyPartition triples in the VoID dataset.
addPropertyPartitions
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addPropertyPartitions sparqlEndpoint = do
  propPartitions <- lift $ selectPropPartitions sparqlEndpoint
  forM_ propPartitions addPropPartition

 where
   addPropPartition :: (Rdf a, Monad m) => (Text, Integer) -> RdfState a m ()
   addPropPartition (propUri, numTriples) = do
     let propPartitionNode = RDF.unode $ datasetUri <> "/PropertyPartition" <> T.pack (show $ abs $ hash propUri)
     addTriple $ Triple (RDF.unode datasetUri)
                        (RDF.unode voidPropertyPartition)
                        propPartitionNode
     addTriple
       $ Triple propPartitionNode (RDF.unode voidProperty) (RDF.unode propUri)
     addTriple $ Triple
       propPartitionNode
       (RDF.unode voidTriples)
       (RDF.lnode $ TypedL (T.pack $ show numTriples) xsdInteger)

-- Query the SPARQL endpoint for the number of triples per property.
--
-- @
-- SELECT ?p (COUNT(?s) as ?count) WHERE {
--   ?s ?p ?o .
-- } GROUPBY ?p
-- @
selectPropPartitions :: (MonadSparqlQuery m)
                      => EndPoint -- ^ Sparql endpoint
                      -> m [(Text, Integer)]
selectPropPartitions sparqlEndpoint = do
  getGroupByCountQueryResult sparqlEndpoint selectPropPartitionsQuery

 where
   selectPropPartitionsQuery :: Query SelectQuery
   selectPropPartitionsQuery = do
      s <- var
      p <- var
      o <- var

      triple_ s p o

      groupBy_ p

      countVar <- var
      select [SelectVar p, count s `as` countVar]

-- |Add all triples related to data linked to Wikidata in the VoID dataset.
addWikidataLinkset
  :: (Rdf a, MonadSparqlQuery m)
  => EndPoint -- ^ SPARQL endpoint
  -> RdfState a m ()
addWikidataLinkset sparqlEndpoint = do
  numOwlSameAsTriples <- lift $ countNumOwlSameAsWikidataTriples sparqlEndpoint

  if numOwlSameAsTriples == 0
  then return ()
  else do
    mapM_ addTriple $ mkTriple wikidataLinksetUri rdfType voidLinkset
    mapM_ addTriple $ mkTriple wikidataLinksetUri voidTarget datasetUri
    mapM_ addTriple $ mkTriple wikidataLinksetUri voidTarget wikidataDatasetUri
    mapM_ addTriple $ mkTriple wikidataLinksetUri voidSubset datasetUri
    mapM_ addTriple $ mkTriple wikidataLinksetUri voidLinkPredicate owlSameAs
    mapM_ addTriple $ mkTripleLit wikidataLinksetUri voidTriples (TypedL (T.pack $ show numOwlSameAsTriples) xsdInteger)

-- |Count number of triples with the owl:sameAs predicate linked to Wikidata.
--
-- @
-- prefix owl: <http://www.w3.org/2002/07/owl#>
-- select (count(?s) as ?count) where {
--   ?s owl:sameAs ?o
--   FILTER (contains(str(?o), "wikidata.org"))
-- }
-- @
countNumOwlSameAsWikidataTriples :: (MonadSparqlQuery m)
                                 => EndPoint -- ^ Sparql endpoint
                                 -> m Integer
countNumOwlSameAsWikidataTriples sparqlEndpoint = do
  getCountQueryResult sparqlEndpoint countNumOwlSameAsWikidataTriplesQuery

countNumOwlSameAsWikidataTriplesQuery :: Query SelectQuery
countNumOwlSameAsWikidataTriplesQuery = do
  s <- var
  o <- var

  triple_ s (iriRef owlSameAs) o

  filterExpr_ $ contains (str o) ("wikidata.org" :: Text)

  countVar <- var
  select [count s `as` countVar]

-- allTriples :: (Rdf a) => RDF a -> Triples
-- allTriples graph = RDF.query graph Nothing Nothing Nothing

-- |Get count query result from SPARQL select query.
--
-- Expects the SPARQL select response to contain a single literal typed as a xsd:integer.
getCountQueryResult :: (MonadSparqlQuery m)
                    => EndPoint -- ^ SPARQL endpoint
                    -> Query SelectQuery
                    -> m Integer
getCountQueryResult sparqlEndpoint query = do
  resultsMaybe <- selectQuery sparqlEndpoint query
  return $ fromMaybe 0 $ getCountFromResults resultsMaybe

 where
  getCountFromResults :: Maybe [[BindingValue]] -> Maybe Integer
  getCountFromResults resultsMaybe = do
    results <- resultsMaybe
    firstRow <- listToMaybe results
    countBindingValue <- listToMaybe firstRow
    countLiteral <- getIntegerLiteralFromBindingValue countBindingValue
    fst <$> eitherToMaybe (T.decimal countLiteral)

  getIntegerLiteralFromBindingValue :: BindingValue -> Maybe Text
  getIntegerLiteralFromBindingValue bindingValue =
    case bindingValue of
      Bound (LNode (TypedL c "http://www.w3.org/2001/XMLSchema#integer")) -> Just c
      _ -> Nothing

-- |Get GROUP BY query result from SPARQL select query.
--
-- Expects the SPARQL select response to contain 2 columns. The 1st represents
-- the grouped element and the 2nd represent the count as a xsd:integer.
getGroupByCountQueryResult :: (MonadSparqlQuery m)
                           => EndPoint -- ^ SPARQL endpoint
                           -> Query SelectQuery
                           -> m [(Text, Integer)]
getGroupByCountQueryResult sparqlEndpoint query = do
  resultsMaybe <- selectQuery sparqlEndpoint query
  return $ fromMaybe [] $ getCountFromResults resultsMaybe

 where
  getCountFromResults :: Maybe [[BindingValue]] -> Maybe [(Text, Integer)]
  getCountFromResults resultsMaybe = do
    results <- resultsMaybe
    mapM getGroupByCount results

  getGroupByCount :: [BindingValue] -> Maybe (Text, Integer)
  getGroupByCount bindingValues =
    case bindingValues of
      [Bound (UNode uri), Bound (LNode (TypedL c "http://www.w3.org/2001/XMLSchema#integer"))] -> do
        countDecimal <- fst <$> eitherToMaybe (T.decimal c)
        return (uri, countDecimal)
      _ -> Nothing

-- |Add prefix mappings for VoID data file.
addVoidPrefixMappings :: (Rdf a, Monad m) => RdfState a m ()
addVoidPrefixMappings = do
  let voidPrefixes =
        ns_mappings [rdf, rdfs, owl, void, dcterms, foaf, wd, xsd, wd, formats]
  addPrefixMappings voidPrefixes True

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe e =
  case e of
    Left _ -> Nothing
    Right v -> Just v
