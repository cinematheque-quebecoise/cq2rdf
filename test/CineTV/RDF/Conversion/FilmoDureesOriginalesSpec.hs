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
module CineTV.RDF.Conversion.FilmoDureesOriginalesSpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.FilmoDureesOriginales (convertFilmoDureesOriginales)
import qualified Data.RDF.Types.Extended                     as RDF
import           Import
import           Namespaces
import Data.RDF.Vocabulary
import           Test.Hspec.Expectations.Extended            (shouldContainElems)

import           Control.Monad.State                         (execStateT)
import           Data.Pool                                   (Pool)
import           Data.RDF                                    (RDF)
import qualified Data.RDF                                    as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto                          hiding (get)
import           Database.Persist.Sqlite                     (SqliteConf (..))
import           Test.Hspec

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertFilmoDureesOriginales pool) emptyGraph

  describe "convertFilmoDureesOriginales" $ do
    it "should create related basic triple types"
      $                    RDF.triplesOf graph
      `shouldContainElems` catMaybes
                             [ RDF.mkTriple "/resource/Duration"
                                            rdfType
                                            crmE55
                             , RDF.mkTriple "/resource/Second"
                                            rdfType
                                            crmE58
                             ]

    it "should create triples representings duration of manifestations" $ do
      let workManifesUri     = "/resource/ManifestationProductType1"
      let publicationExprUri = "/resource/PublicationExpression1"
      let dimensionUri       = "/resource/Dimension3456Seconds"

      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple workManifesUri rdfType frbrooF3
        , RDF.mkTriple workManifesUri frbrooCLR6 publicationExprUri
        , RDF.mkTriple workManifesUri crmP43 dimensionUri
        , RDF.mkTriple dimensionUri rdfType crmE54
        , RDF.mkTriple dimensionUri crmP2 "/resource/Duration"
        , RDF.mkTripleLit dimensionUri
                          crmP90
                          (RDF.TypedL "3456" xsdInteger)
        , RDF.mkTriple dimensionUri crmP91 "/resource/Second"
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
                                   Nothing

    insertKey (toSqlKey 10) $ FilmoDureesOriginales (toSqlKey 1) 57 36
  return pool
