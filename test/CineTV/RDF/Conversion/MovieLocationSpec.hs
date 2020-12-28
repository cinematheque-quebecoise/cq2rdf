-- This file is part of cq3rdf.

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
module CineTV.RDF.Conversion.MovieLocationSpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.MovieLocation  (convertMoviesLocation)
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
  graph <- runIO $ execStateT (convertMoviesLocation pool) emptyGraph

  describe "converting cinetv movie location to RDF"
    $ it
        "should create movie location for each row in table Pays and Filmo_Pays"
    $ do
        let recordingEventUri = "/resource/RecordingEvent1"
        let placeUri          = "/resource/Place10"

        RDF.triplesOf graph `shouldContainElems` catMaybes
          [RDF.mkTriple recordingEventUri SW.crmP7 placeUri]

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 10) $ Pays "Québec"
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
    insert $ Filmo_Pays (toSqlKey 1) (toSqlKey 10)
  return pool
