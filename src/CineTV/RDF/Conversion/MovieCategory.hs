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
module CineTV.RDF.Conversion.MovieCategory
  ( convertMoviesCategory
  )
where

import qualified Data.RDF.Types.Extended      as RDF (mkTriple)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import Data.RDF.Vocabulary
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import qualified Data.RDF                     as RDF
import           Data.RDF.State
import           Database.Esqueleto           hiding (get)

baseUriPath :: Text
baseUriPath = "/resource"

convertMoviesCategory
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
convertMoviesCategory = createTriplesFromMoviesCategory

{-|
Create all triples for representing the language of the work.

Generated triples:

@
for each row in Filmo_GenresCategories
  cmtq:Work{Filmo_GenresCategories.FilmoId} crm:P2_has_type cmtq:GenreCategory{Filmo_GenresCategories.SujetId}
@
-}
createTriplesFromMoviesCategory
  :: (RDF.Rdf rdfImpl, MonadIO m) => Pool SqlBackend -> RdfState rdfImpl m ()
createTriplesFromMoviesCategory pool = do
  filmoGenreCategoryEntities <- getFilmoGenreCategoryEntities pool
  mapM_ createTriplesFromFilmoGenreCategory filmoGenreCategoryEntities

getFilmoGenreCategoryEntities
  :: (MonadIO m) => Pool SqlBackend -> m [Entity Filmo_GenresCategories]
getFilmoGenreCategoryEntities pool =
  liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoGenreCategory, sujet) -> do
        where_
          (   (   sujet
              ^.  SujetId
              ==. filmoGenreCategory
              ^.  Filmo_GenresCategoriesSujetId
              )
          &&. (   filmo
              ^.  FilmoId
              ==. filmoGenreCategory
              ^.  Filmo_GenresCategoriesFilmoId
              )
          )
        return filmoGenreCategory

createTriplesFromFilmoGenreCategory
  :: (RDF.Rdf rdfImpl, Monad m)
  => Entity Filmo_GenresCategories
  -> RdfState rdfImpl m ()
createTriplesFromFilmoGenreCategory filmoGenreCategoryEntity = do
  let filmoId = sqlKeyToText $ filmo_GenresCategoriesFilmoId $ entityVal
        filmoGenreCategoryEntity
  let sujetId = sqlKeyToText $ filmo_GenresCategoriesSujetId $ entityVal
        filmoGenreCategoryEntity
  let workUri          = baseUriPath <> "/Work" <> filmoId
  let genreCategoryUri = baseUriPath <> "/GenreCategory" <> sujetId

  mapM_ addTriple $ RDF.mkTriple workUri crmP2 genreCategoryUri
