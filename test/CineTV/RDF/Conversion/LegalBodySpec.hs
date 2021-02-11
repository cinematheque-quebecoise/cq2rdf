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
module CineTV.RDF.Conversion.LegalBodySpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.LegalBody  (convertLegalBodies)
import           Control.Monad.State              (execStateT)
import           Data.RDF.Vocabulary
import           Import
import           Namespaces

import           Data.Pool                        (Pool)
import           Data.RDF                         (RDF)
import qualified Data.RDF                         as RDF
import qualified Data.RDF.Types.Extended          as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto               hiding (get)
import           Database.Persist.Sqlite          (SqliteConf (..))
import           Test.Hspec
import           Test.Hspec.Expectations.Extended (shouldContainElems)

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertLegalBodies pool) emptyGraph

  describe "converting cinetv legal to RDF"
    $ it
        "should create a legal body for each row in table Sujet which is in Filmo_Generique"
    $ do
        let legalBodyUri            = "/resource/LegalBody100"
        let legalBodyAppellationUri = "/resource/AppellationLegalBody100"
        let legalBodyIdentifierUri  = "/resource/IdentifierLegalBody100"

        RDF.triplesOf graph `shouldContainElems` catMaybes
          [ RDF.mkTriple legalBodyUri rdfType crmE40
          , RDF.mkTripleLit legalBodyUri rdfsLabel (RDF.PlainL "PÈRE FILM")
          , RDF.mkTriple legalBodyUri crmP1 legalBodyAppellationUri
          , RDF.mkTriple legalBodyUri crmP48 legalBodyIdentifierUri
          , RDF.mkTriple legalBodyAppellationUri rdfType crmE41
          , RDF.mkTripleLit legalBodyAppellationUri
                            crmP190
                            (RDF.PlainL "PÈRE FILM")
          , RDF.mkTriple legalBodyIdentifierUri rdfType crmE42
          , RDF.mkTripleLit legalBodyIdentifierUri crmP190 (RDF.PlainL "100")
          ]

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 1000) $ Filmo Nothing
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
                                      Nothing

    insertKey (toSqlKey 100) $ Sujet "PÈRE FILM"
    insertKey (toSqlKey 15) $ Fonction "Société de production"
    insert $ Filmo_Generique (toSqlKey 15)
                             (toSqlKey 1000)
                             (Just $ toSqlKey 100)
                             Nothing

  return pool
