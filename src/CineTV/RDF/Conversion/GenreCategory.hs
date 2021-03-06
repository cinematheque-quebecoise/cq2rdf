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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CineTV.RDF.Conversion.GenreCategory
  ( convertGenreCategories
  )
where

import           Data.RDF.Types.Extended      (mkTriple, mkTripleLit)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import Data.RDF.Vocabulary
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import qualified Data.RDF.Types.Extended      as RDF (mkTriple, mkTripleLit)
import           Database.Esqueleto           hiding (get)

baseUriPath :: Text
baseUriPath = "/resource"

{-|
Create all triples for representing a possible genre of a work.

Generated triples:

@
cmtq:GenreCategory rdf:type crm:E55_Type
cmtq:GenreCategory rdfs:label "GenreCategory"
cmtq:GenreCategory rdfs:comment "Genre cinématographique ou catégorie d'une oeuvre"@fr

for each row in table Sujet where Sujet.SujetId = Filmo_GenresCategories.SujetId
   cmtq:GenreCategory{Sujet.SujetID} rdf:type crm:E55_Type
   cmtq:GenreCategory{Sujet.SujetID} rdfs:label {Sujet.Terme}@fr
   cmtq:GenreCategory{Sujet.SujetID} crm:P2_has_type cmtq:GenreCategory
   cmtq:GenreCategory{Sujet.SujetID} crm:P48_has_preferred_identifier cmtq:IdentifierGenreCategory{Sujet.SujetID}
   cmtq:IdentifierGenreCategory{Sujet.SujetID} crm:P190_has_symbolic_content {Sujet.Terme}

for each row in table GenresCategories_LienWikidata
  cmtq:GenreCategory{GenresCategories_LienWikidata.SujetID} owl:sameAs {GenresCategories_LienWikidata.LienWikidata}
@
-}
convertGenreCategories
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertGenreCategories pool = do
  createGenreCategoryType
  createTriplesFromGenres pool
  createTriplesFromAllGenresCategoriesLienWikidata pool

createGenreCategoryType :: (RDF.Rdf rdfImpl, Monad m) => RdfState rdfImpl m ()
createGenreCategoryType = do
  let genreTypeUri = baseUriPath <> "/GenreCategory"
  mapM_ addTriple $ mkTriple genreTypeUri rdfType crmE55
  mapM_ addTriple
    $ mkTripleLit genreTypeUri rdfsLabel (RDF.PlainL "GenreCategory")
  mapM_ addTriple $ mkTripleLit
    genreTypeUri
    rdfsComment
    (RDF.PlainLL "Genre cinématographique ou catégorie d'une oeuvre" "fr")

createTriplesFromGenres
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromGenres pool = do
  sujetEntities <- getGenreCategorieEntities pool
  mapM_ createTriplesFromGenre sujetEntities

getGenreCategorieEntities :: (MonadIO m) => Pool SqlBackend -> m [Entity Sujet]
getGenreCategorieEntities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmoGenresCategories, sujet) -> do
        where_
          (   sujet
          ^.  SujetId
          ==. filmoGenresCategories
          ^.  Filmo_GenresCategoriesSujetId
          )
        return sujet

createTriplesFromGenre
  :: (RDF.Rdf rdfImpl, Monad m) => Entity Sujet -> RdfState rdfImpl m ()
createTriplesFromGenre sujetEntity = do
  let genreUri           = baseUriPath <> "/GenreCategory" <> genreId
  let identifierGenreUri = baseUriPath <> "/IdentifierGenreCategory" <> genreId

  mapM_ addTriple $ RDF.mkTriple genreUri rdfType crmE55
  mapM_ addTriple $ RDF.mkTriple genreUri crmP2 "/resource/GenreCategory"
  mapM_ addTriple
    $ RDF.mkTripleLit genreUri rdfsLabel (RDF.PlainLL genreLabel "fr")
  mapM_ addTriple $ RDF.mkTriple genreUri crmP48 identifierGenreUri
  mapM_ addTriple
    $ RDF.mkTripleLit identifierGenreUri crmP190 (RDF.PlainL genreId)

 where
  genreId    = sqlKeyToText $ entityKey sujetEntity
  genreLabel = sujetTerme $ entityVal sujetEntity

createTriplesFromAllGenresCategoriesLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromAllGenresCategoriesLienWikidata pool = do
  genresLienWikidataEntities <- getAllGenresCategoriesLienWikidataEntities pool
  mapM_ createTriplesFromGenresCategoriesLienWikidata genresLienWikidataEntities

getAllGenresCategoriesLienWikidataEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity GenresCategories_LienWikidata]
getAllGenresCategoriesLienWikidataEntities pool =
  liftIO $ flip liftSqlPersistMPool pool $ select $ distinct $ from return

createTriplesFromGenresCategoriesLienWikidata
  :: (RDF.Rdf rdfImpl, MonadIO m)
  => Entity GenresCategories_LienWikidata
  -> RdfState rdfImpl m ()
createTriplesFromGenresCategoriesLienWikidata genresLienWdEntity = do
  let wikidataUriMaybe =
        genresCategories_LienWikidataLienWikidata $ entityVal genresLienWdEntity

  case wikidataUriMaybe of
    Just wikidataUri -> do
      let genresCategoriesId =
            sqlKeyToText $ genresCategories_LienWikidataSujetId $ entityVal
              genresLienWdEntity
      let genresCategoryUri =
            baseUriPath <> "/GenreCategory" <> genresCategoriesId
      mapM_ addTriple $ mkTriple genresCategoryUri owlSameAs wikidataUri
    Nothing -> return ()

