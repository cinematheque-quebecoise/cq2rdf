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
module Data.CQLOD.Readers.CineTV
  ( readCineTV
  )
where

import           Data.CQLOD                           (CQLOD (..),
                                                       CQLODStatements)
import           Data.CQLOD.Readers.CineTV.FilmoPays (readFilmoPays)
import           Data.CQLOD.Readers.CineTV.Fonction   (readFonction)
import           Data.CQLOD.Readers.CineTV.Pays       (readPays)
import           Data.CQLOD.Readers.CineTV.Nom       (readNom)
import           Data.CQLOD.Readers.CineTV.Langue       (readLangue)
import           Data.CQLOD.Readers.CineTV.Sujet       (readSujet, readOrganisme)
import           Data.CQLOD.Readers.CineTV.Filmo      (readFilmo)
import           Data.CQLOD.Readers.CineTV.FilmoType      (readFilmoType)
import           Data.CQLOD.Readers.CineTV.FilmoRealisation       (readFilmoRealisation)
import           Data.CQLOD.Readers.CineTV.FilmoDureesOriginales       (readFilmoDureesOriginales)
import           Data.CQLOD.Readers.CineTV.FilmoResume       (readFilmoResume)
import           Data.CQLOD.Readers.CineTV.FilmoTitres       (readFilmoTitres)
import           Data.CQLOD.Readers.CineTV.FilmoLienWikidata       (readFilmoLienWikidata)
import           Data.CQLOD.Readers.CineTV.FilmoGenerique       (readFilmoGenerique)
import           Data.CQLOD.Readers.CineTV.FilmoGenresCategories       (readFilmoGenresCategories)
import           Data.CQLOD.Readers.CineTV.FilmoLangue       (readFilmoLangue)
import           Data.CQLOD.Readers.CineTV.GenresCategoriesLienWikidata       (readGenresCategoriesLienWikidata)
import           Data.CQLOD.Readers.CineTV.LangueLienWikidata       (readLangueLienWikidata)
import           Data.CQLOD.Readers.CineTV.NomLienWikidata       (readNomLienWikidata)
import           Data.CQLOD.Readers.CineTV.PaysLienWikidata       (readPaysLienWikidata)
import           Import                               hiding ((^.))

import           Control.Monad.State                  (execStateT)
import           Data.Pool                            (Pool)
import           Database.Esqueleto                   hiding (get)

readCineTV :: (MonadIO m) => Pool SqlBackend -> m CQLOD
readCineTV pool = CQLOD <$> execStateT (readCineTVStatements pool) []

readCineTVStatements :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readCineTVStatements pool = do
  readFonction pool
  readPays pool
  readNom pool
  readLangue pool
  readSujet pool
  readOrganisme pool
  readFilmo pool
  readFilmoType pool
  readFilmoRealisation pool
  readFilmoDureesOriginales pool
  readFilmoResume pool
  readFilmoTitres pool
  readFilmoLienWikidata pool
  readFilmoPays pool
  readFilmoGenerique pool
  readFilmoGenresCategories pool
  readFilmoLangue pool
  readGenresCategoriesLienWikidata pool
  readLangueLienWikidata pool
  readNomLienWikidata pool
  readPaysLienWikidata pool
