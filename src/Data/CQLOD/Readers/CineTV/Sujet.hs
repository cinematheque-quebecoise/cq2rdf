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
module Data.CQLOD.Readers.CineTV.Sujet
  ( readSujet
  , readOrganisme
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements,
                                               GenreCategory (..),
                                               GenreCategoryId (..),
                                               LegalBody (..), LegalBodyId (..),
                                               addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readSujet :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readSujet pool = do
  sujetEntities <-
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
  mapM_ createGenreCategoryStatements sujetEntities

createGenreCategoryStatements
  :: (Monad m) => Entity Sujet -> CQLODStatements m ()
createGenreCategoryStatements sujetEntity = do
  let sid   = GenreCategoryId $ (sqlKeyToText . entityKey) sujetEntity
  let terme = (sujetTerme . entityVal) sujetEntity
  addStatement $ GenreCategoryDeclaration $ GenreCategory sid terme

readOrganisme :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readOrganisme pool = do
  sujetEntities <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmoGenerique, sujet) -> do
        where_
          (sujet ?. SujetId ==. filmoGenerique ^. Filmo_GeneriqueOrganismeId)
        return sujet

  mapM_ createLegalBodyStatements (catMaybes sujetEntities)

createLegalBodyStatements :: (Monad m) => Entity Sujet -> CQLODStatements m ()
createLegalBodyStatements sujetEntity = do
  let sid   = LegalBodyId $ (sqlKeyToText . entityKey) sujetEntity
  let terme = (sujetTerme . entityVal) sujetEntity
  addStatement $ LegalBodyDeclaration $ LegalBody sid terme
