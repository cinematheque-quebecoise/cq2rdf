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
module CineTV.RDF.Conversion.RoleSpec
  ( spec
  )
where

import           Control.Monad.State              (execStateT)
import           CineTV.RDF.Conversion.Role           (convertRoles)
import           Data.Pool                        (Pool)
import           Data.RDF                         (RDF)
import qualified Data.RDF                         as RDF
import qualified Data.RDF.Types.Extended          as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto               hiding (get)
import           Database.Persist.Sqlite          (SqliteConf (..))
import           Import
import           Namespaces
import qualified SW.Vocabulary                    as SW
import           Test.Hspec
import           Test.Hspec.Expectations.Extended (shouldContainElems)

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertRoles pool) emptyGraph

  describe "converting cinetv roles to RDF" $ do
    it "should create a type representing a role" $ do
      let roleUri = "/resource/Role"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple roleUri SW.rdfType SW.crmE55
        , RDF.mkTripleLit roleUri SW.rdfsLabel (RDF.PlainL "Role")
        , RDF.mkTripleLit
          roleUri
          SW.rdfsComment
          (RDF.PlainLL
            "Role occupé par un agent dans la production d'une oeuvre"
            "fr"
          )
        ]

    it "should create roles for each row in table Fonction" $ do
      let role2Uri = "/resource/Role2"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple role2Uri SW.rdfType SW.crmE55
        , RDF.mkTripleLit role2Uri
                          SW.rdfsLabel
                          (RDF.PlainLL "Interprétation" "fr")
        , RDF.mkTriple role2Uri SW.crmP2 "/resource/Role"
        , RDF.mkTriple role2Uri SW.crmP48 "/resource/IdentifierRole2"
        , RDF.mkTripleLit "/resource/IdentifierRole2"
                          SW.crmP190
                          (RDF.PlainL "2")
        ]

    it "should add a new role representing the movie director (not in CineTV)"
      $                    RDF.triplesOf graph
      `shouldContainElems` catMaybes
                             [ RDF.mkTriple "/resource/Role1"
                                            SW.rdfType
                                            SW.crmE55
                             , RDF.mkTripleLit
                               "/resource/Role1"
                               SW.rdfsLabel
                               (RDF.PlainLL "Réalisation" "fr")
                             ]

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 2) $ Fonction "Interprétation"
  return pool
