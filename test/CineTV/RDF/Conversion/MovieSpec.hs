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
module CineTV.RDF.Conversion.MovieSpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.Movie      (convertMovies)
import           Control.Monad.State              (execStateT)
import           Data.RDF.Vocabulary
import           Import
import           Namespaces (prefixMappings)

import           Data.Pool                        (Pool)
import           Data.RDF                         (RDF)
import qualified Data.RDF                         as RDF
import qualified Data.RDF.Extended                as RDF (mkTriple, mkTripleLit)
import qualified Data.RDF.Namespace               as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto               hiding (get)
import           Database.Persist.Sqlite          (SqliteConf (..))
import           Test.Hspec
import           Test.Hspec.Expectations.Extended (shouldContainElems)

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertMovies pool) emptyGraph

  describe "converting cinetv audiovisual work to RDF" $ do
    it "should create a work for each row in table Filmo" $ do
      let workUri = "/resource/Work1"

      RDF.triplesOf graph
        `shouldContainElems` catMaybes [RDF.mkTriple workUri rdfType frbrooF1]

    it "should create id, label and comment for work" $ do
      let workUri              = "/resource/Work1"
      let identifierWorkUri    = "/resource/IdentifierWork1"
      let originalTitleWorkUri = "/resource/OriginalTitleWork1"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTripleLit workUri
                          rdfsLabel
                          (RDF.PlainL "LES INVASIONS BARBARES")
        , RDF.mkTriple workUri crmP102 originalTitleWorkUri
        , RDF.mkTriple workUri crmP48 identifierWorkUri
        , RDF.mkTriple originalTitleWorkUri rdfType crmE35
        , RDF.mkTripleLit originalTitleWorkUri
                          crmP190
                          (RDF.PlainL "LES INVASIONS BARBARES")
        , RDF.mkTriple identifierWorkUri rdfType crmE42
        , RDF.mkTripleLit identifierWorkUri crmP190 (RDF.PlainL "1")
        ]

    it "should create a budget expression for each work" $ do
      let workUri       = "/resource/Work1"
      let budgetUri     = "/resource/Budget"
      let budgetWorkUri = "/resource/Dimension2300000CAD"
      let cadUri        = "/resource/CanadianDollar"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple workUri crmP43 budgetWorkUri
        , RDF.mkTriple budgetWorkUri rdfType crmE54
        , RDF.mkTriple budgetWorkUri crmP2 budgetUri
        , RDF.mkTripleLit budgetWorkUri
                          rdfsLabel
                          (RDF.PlainL "Dimension2300000CAD")
        , RDF.mkTripleLit budgetWorkUri
                          crmP181
                          (RDF.TypedL "2300000" "xsd:double")
        , RDF.mkTriple budgetWorkUri crmP180 cadUri
        , RDF.mkTriple cadUri rdfType crmE98
        , RDF.mkTripleLit cadUri rdfsLabel (RDF.PlainLL "Dollar canadien" "fr")
        ]

    it "should associate triples related to recording of work" $ do
      let workUri           = "/resource/Work1"
      let recordingWorkUri  = "/resource/RecordingWork1"
      let recordingEventUri = "/resource/RecordingEvent1"
      let recordingUri      = "/resource/Recording1"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple recordingWorkUri rdfType frbrooF21
        , RDF.mkTriple recordingEventUri rdfType frbrooF29
        , RDF.mkTriple recordingUri rdfType frbrooF26
        , RDF.mkTriple recordingWorkUri frbrooR2 workUri
        , RDF.mkTriple recordingEventUri frbrooR22 recordingWorkUri
        , RDF.mkTriple recordingEventUri frbrooR21 recordingUri
        ]

    it "should create publication information" $ do
      let publicationExprUri       = "/resource/PublicationExpression1"
      let publicationEventUri      = "/resource/PublicationEvent1"
      let recordingUri             = "/resource/Recording1"
      let publicProjectionEventUri = "/resource/PublicProjectionEvent1"
      let timespanUri              = "/resource/Time-Span2003-01-01T00:00:00Z"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple publicationExprUri rdfType frbrooF24
        , RDF.mkTriple publicationExprUri crmP165 recordingUri
        , RDF.mkTriple publicationEventUri rdfType frbrooF30
        , RDF.mkTriple publicationEventUri frbrooR24 publicationExprUri
        , RDF.mkTriple publicationEventUri crmP183 publicProjectionEventUri
        , RDF.mkTriple publicProjectionEventUri rdfType crmE7
        , RDF.mkTriple publicProjectionEventUri
                       crmP2
                       "/resource/PublicProjectionEvent"
        , RDF.mkTriple publicProjectionEventUri crmP16 publicationExprUri
        , RDF.mkTriple publicProjectionEventUri crmP4 timespanUri
        , RDF.mkTriple timespanUri crmP82a "2003-01-01T00:00:00Z^^xsd:dateTime"
        ]

    it "should create a date/end of production of work" $ do
      let recordingEventUri = "/resource/RecordingEvent1"
      let timespanUri =
            "/resource/Time-Span2011-01-01T00:00:00Z-2012-01-01T00:00:00Z"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple recordingEventUri crmP4 timespanUri
        , RDF.mkTriple timespanUri crmP82a "2011-01-01T00:00:00Z^^xsd:dateTime"
        , RDF.mkTriple timespanUri crmP82b "2012-01-01T00:00:00Z^^xsd:dateTime"
        ]

    it "should handle apostrophe in title prefix" $ do
      let workUri              = "/resource/Work2"
      let originalTitleWorkUri = "/resource/OriginalTitleWork2"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTripleLit workUri rdfsLabel (RDF.PlainL "L'HOMME DE L'ISLE")
        , RDF.mkTriple workUri crmP102 originalTitleWorkUri
        , RDF.mkTriple originalTitleWorkUri rdfType crmE35
        , RDF.mkTripleLit originalTitleWorkUri
                          crmP190
                          (RDF.PlainL "L'HOMME DE L'ISLE")
        ]

    it "should create a link to wikidata" $ do
      let workUri   = "/resource/Work1"
      let wdWorkUri = RDF.mkUri wd "Q1"
      RDF.triplesOf graph `shouldContainElems` catMaybes
        [RDF.mkTriple workUri owlSameAs wdWorkUri]

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 1) $ Filmo (Just "LES")
                                   (Just "INVASIONS BARBARES")
                                   (Just 2003)
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   (Just "01-01-11")
                                   (Just "01-01-12")
                                   Nothing
                                   (Just 2300000)
                                   Nothing
    insertKey (toSqlKey 2) $ Filmo (Just "L'")
                                   (Just "HOMME DE L'ISLE")
                                   (Just 2003)
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   (Just "01-01-11")
                                   (Just "01-01-12")
                                   Nothing
                                   (Just 2300000)
                                   Nothing
    insert $ Filmo_LienWikidata (toSqlKey 1)
                                (Just "http://www.wikidata.org/entity/Q1")

  return pool
