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
module Conversion.CineTV.PersonSpec
  ( spec
  )
where

import           Control.Monad.State          (execStateT)
import           Conversion.CineTV.Person     (convertPeople)
import           Import
import           Namespaces
import qualified SW.Vocabulary                as SW

import           Data.Pool                    (Pool)
import           Data.RDF                     (RDF)
import qualified Data.RDF                     as RDF
import qualified Data.RDF.Namespace           as RDF
import qualified Data.RDF.Types.Extended      as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto           hiding (get)
import           Database.Persist.Sqlite      (SqliteConf (..))
import           Test.Hspec
import           Test.Hspec.Expectations.Extended (shouldContainElems)

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertPeople pool) emptyGraph

  describe "converting cinetv agents to RDF"
    $ it
        "should create a person for each row in table Nom which is in Filmo_Generique"
    $ do
        let personUri = "/resource/Person100"

        RDF.triplesOf graph `shouldContainElems` catMaybes
          [ RDF.mkTriple personUri SW.rdfType SW.crmE21
          , RDF.mkTripleLit personUri SW.rdfsLabel "Ruggero Maccari"
          , RDF.mkTripleLit personUri SW.foafName "Ruggero Maccari"
          , RDF.mkTripleLit personUri SW.foafGivenName "Ruggero"
          , RDF.mkTripleLit personUri SW.foafFamilyName "Maccari"
          , RDF.mkTriple personUri SW.crmP1 "/resource/AppellationPerson100"
          , RDF.mkTriple personUri SW.crmP48 "/resource/IdentifierPerson100"
          , RDF.mkTriple personUri SW.owlSameAs (RDF.mkUri wd "Q968421")
          , RDF.mkTripleLit "/resource/AppellationPerson100" SW.crmP190 "Ruggero Maccari"
          , RDF.mkTripleLit "/resource/IdentifierPerson100" SW.crmP190 "100"
          ]


emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 100) $ Nom (Just "Maccari") (Just "Ruggero")
    _ <- insert $ Nom_LienWikidata
      (toSqlKey 100)
      (Just "http://www.wikidata.org/entity/Q968421")
    insertKey (toSqlKey 101) $ Nom (Just "Altman") (Just "Robert")

    insertKey (toSqlKey 13) $ Fonction "Interprétation"
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
    insert $ Filmo_Generique (toSqlKey 13)
                             (toSqlKey 1000)
                             Nothing
                             (Just $ toSqlKey 100)

  return pool

