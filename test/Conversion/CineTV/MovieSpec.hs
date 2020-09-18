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
module Conversion.CineTV.MovieSpec
  ( spec
  )
where

import           Control.Monad.State              (execStateT)
import           Conversion.CineTV.Movie          (convertMovies)
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
    it "should create a wor for each row in table Filmo" $ do
      let workUri = "/resource/Work1"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [RDF.mkTriple workUri SW.rdfType SW.frbrooF1]

    it "should create id, label and comment for work" $ do
      let workUri            = "/resource/Work1"
      let identifierWorkUri  = "/resource/IdentifierWork1"
      let appellationWorkUri = "/resource/AppellationWork1"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTripleLit workUri SW.rdfsLabel "Les invasions barbares"
        , RDF.mkTriple workUri SW.crmP1 appellationWorkUri
        , RDF.mkTriple workUri SW.crmP48 identifierWorkUri
        , RDF.mkTripleLit appellationWorkUri SW.crmP190 "Les invasions barbares"
        , RDF.mkTripleLit identifierWorkUri SW.crmP190 "1"
        ]

    it "should create a budget expression for each work" $ do
      let workUri       = "/resource/Work1"
      let budgetUri     = "/resource/Budget"
      let budgetWorkUri = "/resource/Dimension2300000CAD"
      let cadUri        = "/resource/CanadianDollar"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple workUri SW.crmP43 budgetWorkUri
        , RDF.mkTriple budgetWorkUri SW.rdfType SW.crmE55
        , RDF.mkTriple budgetWorkUri SW.crmP2 budgetUri
        , RDF.mkTripleLit budgetWorkUri SW.rdfsLabel "Dimension2300000CAD"
        , RDF.mkTripleLit budgetWorkUri SW.crmP181 "2300000^^xsd:double"
        , RDF.mkTriple budgetWorkUri SW.crmP180 cadUri
        , RDF.mkTriple cadUri SW.rdfType SW.crmE98
        , RDF.mkTripleLit cadUri SW.rdfsLabel "Dollar canadien@fr"
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
        , RDF.mkTriple recordingEventUri SW.frbrooR22 workUri
        , RDF.mkTriple recordingEventUri SW.frbrooR21 recordingUri
        ]

    it "should create publication information" $ do
      let publicationExprUri = "/resource/PublicationExpression1"
      let recordingUri       = "/resource/Recording1"
      let premiereUri        = "/resource/Premiere1"
      let timespanUri        = "/resource/Time-Span2003-01-01T00:00:00Z"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple publicationExprUri SW.rdfType SW.frbrooF24
        , RDF.mkTriple recordingUri SW.crmP165 publicationExprUri
        , RDF.mkTriple premiereUri SW.rdfType SW.crmE7
        , RDF.mkTriple premiereUri SW.crmP16 publicationExprUri
        , RDF.mkTriple premiereUri SW.crmP4 timespanUri
        , RDF.mkTriple timespanUri
                       SW.crmP82a
                       "2003-01-01T00:00:00Z^^xsd:datetime"
        ]

    it "should create a date/end of production of work" $ do
      let recordingEventUri = "/resource/RecordingEvent1"
      let timespanUri =
            "/resource/Time-Span2011-01-01T00:00:00Z-2012-01-01T00:00:00Z"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple recordingEventUri SW.crmP4 timespanUri
        , RDF.mkTriple timespanUri
                       SW.crmP82a
                       "2011-01-01T00:00:00Z^^xsd:datetime"
        , RDF.mkTriple timespanUri
                       SW.crmP82b
                       "2012-01-01T00:00:00Z^^xsd:datetime"
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
    insertKey (toSqlKey 1) $ Filmo Nothing
                                   (Just "Les invasions barbares")
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
