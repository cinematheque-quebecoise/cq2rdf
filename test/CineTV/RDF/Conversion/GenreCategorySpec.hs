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
module CineTV.RDF.Conversion.GenreCategorySpec
  ( spec
  )
where

import           CineTV.RDF.Conversion.GenreCategory (convertGenreCategories)
import qualified Data.RDF.Types.Extended             as RDF
import           Import
import           Namespaces (prefixMappings)
import Data.RDF.Vocabulary
import           Test.Hspec.Expectations.Extended    (shouldContainElems)

import           Control.Monad.State                 (execStateT)
import           Data.Pool                           (Pool)
import           Data.RDF                            (RDF)
import qualified Data.RDF                            as RDF
import qualified Data.RDF.Namespace                  as RDF
import           Database.CineTv.Public.Model
import           Database.Esqueleto                  hiding (get)
import           Database.Persist.Sqlite             (SqliteConf (..))
import           Test.Hspec

spec :: Spec
spec = do
  pool  <- runIO dbSetup
  graph <- runIO $ execStateT (convertGenreCategories pool) emptyGraph

  describe "converting cinetv genres to RDF" $ do
    it "should create a type representing a genre" $ do
      let genreUri = "/resource/GenreCategory"
      RDF.triplesOf graph `shouldContainElems` catMaybes
        [ RDF.mkTriple genreUri rdfType crmE55
        , RDF.mkTripleLit
          genreUri
          rdfsComment
          (RDF.PlainLL "Genre cinématographique ou catégorie d'une oeuvre" "fr")
        ]

    it
        "should create movie categories for each row in table Sujet and Filmo_GenresCategories"
      $ do
          let genreUri            = "/resource/GenreCategory"
          let genre1Uri           = "/resource/GenreCategory1"
          let identifierGenre1Uri = "/resource/IdentifierGenreCategory1"

          RDF.triplesOf graph `shouldContainElems` catMaybes
            [ RDF.mkTriple genre1Uri rdfType crmE55
            , RDF.mkTripleLit genre1Uri rdfsLabel (RDF.PlainLL "Drame" "fr")
            , RDF.mkTriple genre1Uri owlSameAs (RDF.mkUri wd "Q130232")
            , RDF.mkTriple genre1Uri crmP2 genreUri
            , RDF.mkTriple genre1Uri crmP48 identifierGenre1Uri
            , RDF.mkTripleLit identifierGenre1Uri crmP190 (RDF.PlainL "1")
            ]

emptyGraph :: RDF RDF.TList
emptyGraph = RDF.mkRdf [] Nothing prefixMappings

dbSetup :: IO (Pool SqlBackend)
dbSetup = do
  pool <- createPoolConfig (SqliteConf ":memory:" 1)
  _    <- liftIO $ flip liftSqlPersistMPool pool $ do
    runMigration migrateAll
    insertKey (toSqlKey 1) $ Sujet "Drame"
    _ <- insert $ GenresCategories_LienWikidata
      (toSqlKey 1)
      (Just "http://www.wikidata.org/entity/Q130232")
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
    insert $ Filmo_GenresCategories (toSqlKey 1000) (toSqlKey 1)
  return pool
