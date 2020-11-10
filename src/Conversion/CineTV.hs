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
module Conversion.CineTV
  ( convertToRdf
  )
where

import Import hiding ((^.))
import Conversion.CineTV.Role (convertRoles)
import Conversion.CineTV.Person (convertPeople)
import Conversion.CineTV.LegalBody (convertLegalBodies)
import Conversion.CineTV.GenreCategory (convertGenreCategories)
import Conversion.CineTV.Place (convertPlaces)
import Conversion.CineTV.Language (convertLanguages)
import Conversion.CineTV.Movie (convertMovies)
import Conversion.CineTV.MovieLocation (convertMoviesLocation)
import Conversion.CineTV.MovieCategory (convertMoviesCategory)
import Conversion.CineTV.MovieDirector (convertMoviesDirector)
import Conversion.CineTV.MovieGeneric (convertMoviesGeneric)
import Conversion.CineTV.MovieLanguage (convertMoviesLanguage)
import Conversion.CineTV.MovieResume (convertMoviesResume)
import Conversion.CineTV.FilmoDureesOriginales (convertFilmoDureesOriginales)
import Conversion.CineTV.FilmoTitres (convertFilmoTitres)
import Data.RDF.State

import qualified Data.RDF as RDF
import Database.Esqueleto hiding (get)
import Data.Pool (Pool)

convertToRdf :: (MonadIO m)
           => Pool SqlBackend
           -> RdfState RDF.TList m ()
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
  convertMoviesResume pool
  convertFilmoTitres pool
  convertFilmoDureesOriginales pool
