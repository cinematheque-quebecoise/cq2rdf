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
module CineTV.RDF.Conversion.MovieGenericSpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.MovieGeneric (convertMoviesGeneric)
import           Control.Monad.State                (execStateT)
import qualified Data.RDF.Types.Extended            as RDF (mkTriple)
import           Data.RDF.Vocabulary
import           Import
import           Namespaces
import           Test.Hspec.Expectations.Extended   (shouldContainElems)

import           Data.Pool                          (Pool)
import           Data.RDF                           (RDF)
import qualified Data.RDF                           as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto                 hiding (get)
import           Database.Persist.Sqlite            (SqliteConf (..))
import           Test.Hspec

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertMoviesGeneric pool) emptyGraph

  describe "converting cinetv movie generics to RDF" $ do
    let recordingEventUri = "/resource/RecordingEvent1"

    it "should create a role activity for roles present in generic" $ do
      let realisationActivityUri = "/resource/RecordingActivity1"
      let realisationActivityCarriedOutByUri =
            "/resource/RecordingActivityCarriedOutBy1"
      let personUri = "/resource/Person1"
      let roleUri   = "/resource/Role1"
      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple recordingEventUri crmP9 realisationActivityUri
        , RDF.mkTriple realisationActivityCarriedOutByUri
                       crmP01
                       realisationActivityUri
        , RDF.mkTriple realisationActivityCarriedOutByUri crmP02 personUri
        , RDF.mkTriple realisationActivityCarriedOutByUri crmP14_1 roleUri
        ]

    it "should detect if the activity if occupied by legal body or person" $ do
      let realisationActivityCarriedOutByUri =
            "/resource/RecordingActivityCarriedOutBy2"
      let legalBodyUri = "/resource/LegalBody2"
      let roleUri      = "/resource/Role11"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple realisationActivityCarriedOutByUri crmP02 legalBodyUri
        , RDF.mkTriple realisationActivityCarriedOutByUri crmP14_1 roleUri
        ]

    it "should handle the role 33 (Source Originale) for creating deriving work"
      $ do
          let workUri                  = "/resource/Work1"
          let workDerivedUri           = "/resource/Work1-3"
          let workDerivedConceptionUri = "/resource/WorkConception1-3"
          let personUri                = "/resource/Person2"

          RDF.triplesOf graph `shouldContainElems` catMaybes
            [ RDF.mkTriple workUri frbrooR2 workDerivedUri
            , RDF.mkTriple workDerivedUri frbrooR16i workDerivedConceptionUri
            , RDF.mkTriple workDerivedConceptionUri crmP14 personUri
            ]

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 1) $ Filmo Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing

    insertKey (toSqlKey 1) $ Fonction "Réalisation"
    insertKey (toSqlKey 1) $ Nom (Just "Michel") (Just "Brault")
    insertKey (toSqlKey 1)
      $ Filmo_Generique (toSqlKey 1) (toSqlKey 1) Nothing (Just $ toSqlKey 1)

    insertKey (toSqlKey 11) $ Fonction "Société de production"
    insertKey (toSqlKey 2) $ Sujet "ALLIANCE VIVAFILM"
    insertKey (toSqlKey 2)
      $ Filmo_Generique (toSqlKey 11) (toSqlKey 1) (Just $ toSqlKey 2) Nothing

    insertKey (toSqlKey 33) $ Fonction "Source originale"
    insertKey (toSqlKey 2) $ Nom (Just "Patrick") (Just "Sénécal")
    insertKey (toSqlKey 3)
      $ Filmo_Generique (toSqlKey 33) (toSqlKey 1) Nothing (Just $ toSqlKey 2)

  return pool

