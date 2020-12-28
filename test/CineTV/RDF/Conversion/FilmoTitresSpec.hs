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
module CineTV.RDF.Conversion.FilmoTitresSpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.FilmoTitres    (convertFilmoTitres)
import qualified Data.RDF.Types.Extended          as RDF
import           Import
import           Namespaces
import qualified SW.Vocabulary                    as SW
import           Test.Hspec.Expectations.Extended (shouldContainElems)

import           Control.Monad.State              (execStateT)
import           Data.Pool                        (Pool)
import           Data.RDF                         (RDF)
import qualified Data.RDF                         as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto               hiding (get)
import           Database.Persist.Sqlite          (SqliteConf (..))
import           Test.Hspec

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertFilmoTitres pool) emptyGraph

  describe "convertFilmoTitres" $ do
    it "should create triples representings all work titles from work" $ do
      let workUri      = "/resource/Work1"
      let workTitleUri = "/resource/WorkTitle10"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple workUri SW.crmP102 workTitleUri
        , RDF.mkTriple workTitleUri SW.rdfType SW.crmE35
        , RDF.mkTripleLit workTitleUri
                          SW.crmP190
                          (RDF.PlainL "LES INVASIONS BARBARES")
        ]

    it "should handle apostrophe in prefixe" $ do
      let workUri      = "/resource/Work2"
      let workTitleUri = "/resource/WorkTitle11"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple workUri SW.crmP102 workTitleUri
        , RDF.mkTriple workTitleUri SW.rdfType SW.crmE35
        , RDF.mkTripleLit workTitleUri
                          SW.crmP190
                          (RDF.PlainL "L'HOMME DE L'ISLE")
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
    insertKey (toSqlKey 2) $ Filmo Nothing
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
    insertKey (toSqlKey 1) $ TypeTitre "Autre titre"
    insertKey (toSqlKey 10) $ FilmoTitres (toSqlKey 1)
                                          (toSqlKey 1)
                                          (Just "LES")
                                          "INVASIONS BARBARES"
    insertKey (toSqlKey 11)
      $ FilmoTitres (toSqlKey 2) (toSqlKey 1) (Just "L'") "HOMME DE L'ISLE"

  return pool
