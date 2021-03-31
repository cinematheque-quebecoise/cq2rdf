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
module CineTV.RDF.Conversion
  ( convertToRdf
  )
where

import           CineTV.RDF.Conversion.FilmoDureesOriginales (convertFilmoDureesOriginales)
import           CineTV.RDF.Conversion.FilmoTitres           (convertFilmoTitres)
import           CineTV.RDF.Conversion.GenreCategory         (convertGenreCategories)
import           CineTV.RDF.Conversion.Language              (convertLanguages)
import           CineTV.RDF.Conversion.LegalBody             (convertLegalBodies)
import           CineTV.RDF.Conversion.Movie                 (convertMovies)
import           CineTV.RDF.Conversion.MovieCategory         (convertMoviesCategory)
import           CineTV.RDF.Conversion.MovieDirector         (convertMoviesDirector)
import           CineTV.RDF.Conversion.MovieGeneric          (convertMoviesGeneric)
import           CineTV.RDF.Conversion.MovieLanguage         (convertMoviesLanguage)
import           CineTV.RDF.Conversion.MovieLocation         (convertMoviesLocation)
import           CineTV.RDF.Conversion.Person                (convertPeople)
import           CineTV.RDF.Conversion.Place                 (convertPlaces)
import           CineTV.RDF.Conversion.Role                  (convertRoles)
import           Data.RDF.State
import           Import                                      hiding ((^.))

import           Data.Pool                                   (Pool)
import           Data.RDF                                    (Rdf)
import           Database.Esqueleto                          hiding (get)

convertToRdf
  :: (MonadIO m, Rdf rdfImpl) => Pool SqlBackend -> RdfState rdfImpl m ()
convertToRdf pool = do
  convertRoles pool
  convertPeople pool
  convertLegalBodies pool
  convertGenreCategories pool
  convertPlaces pool
  convertLanguages pool
  convertMovies pool
  convertMoviesLocation pool
  convertMoviesCategory pool
  convertMoviesDirector pool
  convertMoviesGeneric pool
  convertMoviesLanguage pool
  convertFilmoTitres pool
  convertFilmoDureesOriginales pool
