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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CineTV.RDF.VoidSpec
  ( spec
  )
where

-- import           CineTV.RDF.Void                  (MonadSparqlQuery(..),CinetvRdf (..))
-- import           Data.RDF.Types.Extended          (bnodeGen)
import           Import

import           Control.Monad.State (execStateT)
-- import           Data.Hashable                    (Hashable (..))
-- import qualified Data.Map                         as M
import           Data.RDF            (LValue (..), TList, Triple (..), empty,
                                      lnode, unode)
import           Data.RDF.State      (RdfState)
import qualified Data.RDF.State      as RS
-- import qualified Data.Set                         as S
-- import           Data.Time                        (UTCTime)
-- import           Data.Time.Format.ISO8601         (iso8601ParseM)
-- import           SW.Vocabulary
import           Test.Hspec
-- import           Test.Hspec.Expectations.Extended (shouldContainElems)
-- import Database.HSparql.QueryGenerator (Query, SelectQuery)

-- instance MonadSparqlQuery (State (RDF TList)) where
--   selectQuery endpoint query = do
--     graph <- get
--     return Nothing

spec :: Spec
spec = do
  _ <- execStateT mkCinetvRdfGraph empty
  -- snapshotTime <- runIO (iso8601ParseM "2019-07-25T00:00:00Z" :: IO UTCTime)
  -- issuedTime   <- runIO (iso8601ParseM "2021-01-04T10:30:00Z" :: IO UTCTime)
  -- let cinetvRdf  = CinetvRdf snapshotTime issuedTime "http://localhost:9999/sparql"

  return ()
  -- voidGraph <- (createVoidGraph cinetvRdf :: m (RDF AlgebraicGraph))

  -- let datasetUri = "/void/Dataset#CinemathequeQuebecoiseLinkedOpenDatabase"

  -- describe "CinetvRdf createVoidGraph" $ do
  --   it "should add correct prefixes" $ do
  --     let (PrefixMappings mappings) = prefixMappings voidGraph
  --     M.keysSet mappings `shouldBe` S.fromList
  --       [ "void"
  --       , "rdf"
  --       , "rdfs"
  --       , "owl"
  --       , "xsd"
  --       , "dcterms"
  --       , "wd"
  --       , "formats"
  --       , "foaf"
  --       ]

  --   it "should add dcterms:created triple" $ do
  --     let datasetCreatedTriple = Triple
  --           (unode datasetUri)
  --           (unode dctermsCreated)
  --           (lnode $ TypedL "2019-07-25T00:00:00Z" xsdDateTime)

  --     triplesOf voidGraph `shouldContainElems` [datasetCreatedTriple]

  --   it "should add dcterms:issued triple" $ do
  --     let datasetCreatedTriple = Triple
  --           (unode datasetUri)
  --           (unode dctermsIssued)
  --           (lnode $ TypedL "2021-01-04T10:30:00Z" xsdDateTime)

  --     triplesOf voidGraph `shouldContainElems` [datasetCreatedTriple]

    -- it "should add void:triples triple" $ do
    --   let numTriplesTriple = Triple (unode datasetUri)
    --                                 (unode voidTriples)
    --                                 (lnode $ TypedL "6" xsdInteger)

    --   triplesOf voidGraph `shouldContainElems` [numTriplesTriple]

    -- it "should add void:entities triple" $ do
    --   let numEntitiesTriple = Triple (unode datasetUri)
    --                                  (unode voidEntities)
    --                                  (lnode $ TypedL "3" xsdInteger)

    --   triplesOf voidGraph `shouldContainElems` [numEntitiesTriple]

    -- it "should add void:distinctSubjects triple" $ do
    --   let numDistSubjsTriple = Triple (unode datasetUri)
    --                                   (unode voidDistinctSubjects)
    --                                   (lnode $ TypedL "3" xsdInteger)

    --   triplesOf voidGraph `shouldContainElems` [numDistSubjsTriple]

    -- it "should add void:distinctObjects triple" $ do
    --   let numDistSubjsTriple = Triple (unode datasetUri)
    --                                   (unode voidDistinctObjects)
    --                                   (lnode $ TypedL "4" xsdInteger)

    --   triplesOf voidGraph `shouldContainElems` [numDistSubjsTriple]

    -- it "should add void:classes triple" $ do
    --   let numClassesTriple = Triple (unode datasetUri)
    --                                 (unode voidClasses)
    --                                 (lnode $ TypedL "2" xsdInteger)

    --   triplesOf voidGraph `shouldContainElems` [numClassesTriple]

    -- it "should add void:properties triple" $ do
    --   let numPropsTriple = Triple (unode datasetUri)
    --                               (unode voidProperties)
    --                               (lnode $ TypedL "4" xsdInteger)

    --   triplesOf voidGraph `shouldContainElems` [numPropsTriple]

    -- it "should add all void:classPartition triples" $ do
    --   let crmE53Partition = bnodeGen $ hash crmE53
    --   let crmE41Partition = bnodeGen $ hash crmE41
    --   triplesOf voidGraph
    --     `shouldContainElems` [ Triple (unode datasetUri)
    --                                   (unode voidClassPartition)
    --                                   crmE53Partition
    --                          , Triple crmE53Partition
    --                                   (unode voidClass)
    --                                   (unode crmE53)
    --                          , Triple crmE53Partition
    --                                   (unode voidEntities)
    --                                   (lnode $ TypedL "2" xsdInteger)
    --                          , Triple (unode datasetUri)
    --                                   (unode voidClassPartition)
    --                                   crmE41Partition
    --                          , Triple crmE41Partition
    --                                   (unode voidClass)
    --                                   (unode crmE41)
    --                          , Triple crmE41Partition
    --                                   (unode voidEntities)
    --                                   (lnode $ TypedL "1" xsdInteger)
    --                          ]

    -- it "should add all void:propertyPartition triples" $ do
    --   let rdfTypePartition   = bnodeGen $ hash rdfType
    --   let crmP1Partition     = bnodeGen $ hash crmP1
    --   let crmP190Partition   = bnodeGen $ hash crmP190
    --   let owlSameAsPartition = bnodeGen $ hash owlSameAs
    --   triplesOf voidGraph
    --     `shouldContainElems` [ Triple (unode datasetUri)
    --                                   (unode voidPropertyPartition)
    --                                   rdfTypePartition
    --                          , Triple rdfTypePartition
    --                                   (unode voidProperty)
    --                                   (unode rdfType)
    --                          , Triple rdfTypePartition
    --                                   (unode voidTriples)
    --                                   (lnode $ TypedL "3" xsdInteger)
    --                          , Triple (unode datasetUri)
    --                                   (unode voidPropertyPartition)
    --                                   crmP1Partition
    --                          , Triple crmP1Partition
    --                                   (unode voidProperty)
    --                                   (unode crmP1)
    --                          , Triple crmP1Partition
    --                                   (unode voidTriples)
    --                                   (lnode $ TypedL "1" xsdInteger)
    --                          , Triple (unode datasetUri)
    --                                   (unode voidPropertyPartition)
    --                                   crmP190Partition
    --                          , Triple crmP190Partition
    --                                   (unode voidProperty)
    --                                   (unode crmP190)
    --                          , Triple crmP190Partition
    --                                   (unode voidTriples)
    --                                   (lnode $ TypedL "1" xsdInteger)
    --                          , Triple (unode datasetUri)
    --                                   (unode voidPropertyPartition)
    --                                   owlSameAsPartition
    --                          , Triple owlSameAsPartition
    --                                   (unode voidProperty)
    --                                   (unode owlSameAs)
    --                          , Triple owlSameAsPartition
    --                                   (unode voidTriples)
    --                                   (lnode $ TypedL "1" xsdInteger)
    --                          ]

    -- it "should add Wikidata Linkset related triples" $ do
    --   let linksetUri =
    --         "/void/Dataset#Wikidata_CinemathequeQuebecoiseLinkedOpenDatabase"
    --   let wikidataDatasetUri = "/void/Dataset#Wikidata"
    --   triplesOf voidGraph
    --     `shouldContainElems` [ Triple (unode linksetUri)
    --                                   (unode rdfType)
    --                                   (unode voidLinkset)
    --                          , Triple (unode linksetUri)
    --                                   (unode voidTarget)
    --                                   (unode datasetUri)
    --                          , Triple (unode linksetUri)
    --                                   (unode voidTarget)
    --                                   (unode wikidataDatasetUri)
    --                          , Triple (unode linksetUri)
    --                                   (unode voidSubset)
    --                                   (unode datasetUri)
    --                          , Triple (unode linksetUri)
    --                                   (unode voidLinkPredicate)
    --                                   (unode owlSameAs)
    --                          , Triple (unode linksetUri)
    --                                   (unode voidTriples)
    --                                   (lnode $ TypedL "1" xsdInteger)
    --                          ]

mkCinetvRdfGraph :: (Monad m) => RdfState TList m ()
mkCinetvRdfGraph = do
  RS.addTriple $ Triple
    (unode "/resource/Place77")
    (unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    (unode "http://www.cidoc-crm.org/cidoc-crm/E53_Place")
  RS.addTriple $ Triple
    (unode "/resource/Place100")
    (unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    (unode "http://www.cidoc-crm.org/cidoc-crm/E53_Place")
  RS.addTriple $ Triple
    (unode "/resource/Place77")
    (unode "http://www.cidoc-crm.org/cidoc-crm/P1_is_identified_by")
    (unode "/resource/AppellationPlace77")
  RS.addTriple $ Triple (unode "/resource/Place77")
                        (unode "http://www.w3.org/2002/07/owl#sameAs")
                        (unode "http://www.wikidata.org/entity/Q423")
  RS.addTriple $ Triple
    (unode "/resource/AppellationPlace77")
    (unode "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    (unode "http://www.cidoc-crm.org/cidoc-crm/E41_Appellation")
  RS.addTriple $ Triple
    (unode "/resource/AppellationPlace77")
    (unode "http://www.cidoc-crm.org/cidoc-crm/P190_has_symbolic_content")
    (lnode $ PlainL "Corée, République populaire démocratique de")
