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
import           Import
import           Namespaces
import qualified SW.Vocabulary                    as SW

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

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [RDF.mkTriple workUri SW.rdfType SW.frbrooF1]

    it "should create id, label and comment for work" $ do
      let workUri              = "/resource/Work1"
      let identifierWorkUri    = "/resource/IdentifierWork1"
      let originalTitleWorkUri = "/resource/OriginalTitleWork1"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTripleLit workUri
                          SW.rdfsLabel
                          (RDF.PlainL "LES INVASIONS BARBARES")
        , RDF.mkTriple workUri SW.crmP102 originalTitleWorkUri
        , RDF.mkTriple workUri SW.crmP48 identifierWorkUri
        , RDF.mkTriple originalTitleWorkUri SW.rdfType SW.crmE35
        , RDF.mkTripleLit originalTitleWorkUri
                          SW.crmP190
                          (RDF.PlainL "LES INVASIONS BARBARES")
        , RDF.mkTriple identifierWorkUri SW.rdfType SW.crmE42
        , RDF.mkTripleLit identifierWorkUri SW.crmP190 (RDF.PlainL "1")
        ]

    it "should create a budget expression for each work" $ do
      let workUri       = "/resource/Work1"
      let budgetUri     = "/resource/Budget"
      let budgetWorkUri = "/resource/Dimension2300000CAD"
      let cadUri        = "/resource/CanadianDollar"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple workUri SW.crmP43 budgetWorkUri
        , RDF.mkTriple budgetWorkUri SW.rdfType SW.crmE54
        , RDF.mkTriple budgetWorkUri SW.crmP2 budgetUri
        , RDF.mkTripleLit budgetWorkUri
                          SW.rdfsLabel
                          (RDF.PlainL "Dimension2300000CAD")
        , RDF.mkTripleLit budgetWorkUri
                          SW.crmP181
                          (RDF.TypedL "2300000" "xsd:double")
        , RDF.mkTriple budgetWorkUri SW.crmP180 cadUri
        , RDF.mkTriple cadUri SW.rdfType SW.crmE98
        , RDF.mkTripleLit cadUri
                          SW.rdfsLabel
                          (RDF.PlainLL "Dollar canadien" "fr")
        ]

    it "should associate triples related to recording of work" $ do
      let workUri           = "/resource/Work1"
      let recordingWorkUri  = "/resource/RecordingWork1"
      let recordingEventUri = "/resource/RecordingEvent1"
      let recordingUri      = "/resource/Recording1"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple recordingWorkUri SW.rdfType SW.frbrooF21
        , RDF.mkTriple recordingEventUri SW.rdfType SW.frbrooF29
        , RDF.mkTriple recordingUri SW.rdfType SW.frbrooF26
        , RDF.mkTriple recordingWorkUri SW.frbrooR2 workUri
        , RDF.mkTriple recordingEventUri SW.frbrooR22 recordingWorkUri
        , RDF.mkTriple recordingEventUri SW.frbrooR21 recordingUri
        ]

    it "should create publication information" $ do
      let publicationExprUri       = "/resource/PublicationExpression1"
      let publicationEventUri      = "/resource/PublicationEvent1"
      let recordingUri             = "/resource/Recording1"
      let publicProjectionEventUri = "/resource/PublicProjectionEvent1"
      let timespanUri              = "/resource/Time-Span2003-01-01T00:00:00Z"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple publicationExprUri SW.rdfType SW.frbrooF24
        , RDF.mkTriple publicationExprUri SW.crmP165 recordingUri
        , RDF.mkTriple publicationEventUri SW.rdfType SW.frbrooF30
        , RDF.mkTriple publicationEventUri SW.frbrooR24 publicationExprUri
        , RDF.mkTriple publicationEventUri SW.crmP183 publicProjectionEventUri
        , RDF.mkTriple publicProjectionEventUri SW.rdfType SW.crmE7
        , RDF.mkTriple publicProjectionEventUri
                       SW.crmP2
                       "/resource/PublicProjectionEvent"
        , RDF.mkTriple publicProjectionEventUri SW.crmP16 publicationExprUri
        , RDF.mkTriple publicProjectionEventUri SW.crmP4 timespanUri
        , RDF.mkTriple timespanUri
                       SW.crmP82a
                       "2003-01-01T00:00:00Z^^xsd:dateTime"
        ]

    it "should create a date/end of production of work" $ do
      let recordingEventUri = "/resource/RecordingEvent1"
      let timespanUri =
            "/resource/Time-Span2011-01-01T00:00:00Z-2012-01-01T00:00:00Z"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple recordingEventUri SW.crmP4 timespanUri
        , RDF.mkTriple timespanUri
                       SW.crmP82a
                       "2011-01-01T00:00:00Z^^xsd:dateTime"
        , RDF.mkTriple timespanUri
                       SW.crmP82b
                       "2012-01-01T00:00:00Z^^xsd:dateTime"
        ]

    it "should handle apostrophe in title prefix" $ do
      let workUri              = "/resource/Work2"
      let originalTitleWorkUri = "/resource/OriginalTitleWork2"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTripleLit workUri SW.rdfsLabel (RDF.PlainL "L'HOMME DE L'ISLE")
        , RDF.mkTriple workUri SW.crmP102 originalTitleWorkUri
        , RDF.mkTriple originalTitleWorkUri SW.rdfType SW.crmE35
        , RDF.mkTripleLit originalTitleWorkUri
                          SW.crmP190
                          (RDF.PlainL "L'HOMME DE L'ISLE")
        ]

    it "should create a link to wikidata" $ do
      let workUri   = "/resource/Work1"
      let wdWorkUri = RDF.mkUri wd "Q1"
      RDF.triplesOf graph `shouldContainElems` catMaybes
        [RDF.mkTriple workUri SW.owlSameAs wdWorkUri]

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
                                   (Just "Résumé en français")
                                   (Just "Resume in english")
                                   (Just "01-01-11")
                                   (Just "01-01-12")
                                   Nothing
                                   (Just 2300000)
    insertKey (toSqlKey 2) $ Filmo (Just "L'")
                                   (Just "HOMME DE L'ISLE")
                                   (Just 2003)
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   (Just "Résumé en français")
                                   (Just "Resume in english")
                                   (Just "01-01-11")
                                   (Just "01-01-12")
                                   Nothing
                                   (Just 2300000)
    insert $ Filmo_LienWikidata (toSqlKey 1)
                                (Just "http://www.wikidata.org/entity/Q1")

  return pool
