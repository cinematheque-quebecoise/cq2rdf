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
module Conversion.CineTV.MovieDirectorSpec
  ( spec
  )
where

import           Conversion.CineTV.MovieDirector  (convertMoviesDirector)
import qualified Data.RDF.Types.Extended          as RDF (mkTriple)
import           Import
import           Namespaces
import qualified SW.Vocabulary                    as SW

import           Control.Monad.State              (execStateT)
import           Data.Pool                        (Pool)
import           Data.RDF                         (RDF)
import qualified Data.RDF                         as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto               hiding (get)
import           Database.Persist.Sqlite          (SqliteConf (..))
import           Test.Hspec
import           Test.Hspec.Expectations.Extended (shouldContainElems)

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertMoviesDirector pool) emptyGraph

  describe "converting cinetv movie director to RDF"
    $ it
        "should create movie director for each row in Filmo_Realisation"
    $ do
      let recordingEventUri = "/resource/RecordingEvent1"
      let realisationActivityUri = "/resource/RecordingActivityDirector1-10"
      let realisationActivityCarriedOutByUri = "/resource/RecordingActivityCarriedOutByDirector1-10"
      let personUri = "/resource/Person10"
      let roleUri = "/resource/Role1"
      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple recordingEventUri SW.crmP9 realisationActivityUri
        , RDF.mkTriple realisationActivityCarriedOutByUri SW.crmP01 realisationActivityUri
        , RDF.mkTriple realisationActivityCarriedOutByUri SW.crmP02 personUri
        , RDF.mkTriple realisationActivityCarriedOutByUri SW.crmP14_1 roleUri
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
    insertKey (toSqlKey 10) $ Nom (Just "Michel") (Just "Brault")
    insert $ Filmo_Realisation (toSqlKey 1) (toSqlKey 10)
  return pool
