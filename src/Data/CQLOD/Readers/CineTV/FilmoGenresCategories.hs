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
module Data.CQLOD.Readers.CineTV.FilmoGenresCategories
  ( readFilmoGenresCategories
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements,
                                               GenreCategoryId (..),
                                               WorkId (..), addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readFilmoGenresCategories
  :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoGenresCategories pool = do
  filmoGenresCategoriesEntities <-
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
  mapM_ createStatements filmoGenresCategoriesEntities

createStatements
  :: (Monad m) => Entity Filmo_GenresCategories -> CQLODStatements m ()
createStatements filmoGenresCategoriesEntity = do
  let filmoId = sqlKeyToText $ filmo_GenresCategoriesFilmoId $ entityVal
        filmoGenresCategoriesEntity
  let sujetId = sqlKeyToText $ filmo_GenresCategoriesSujetId $ entityVal
        filmoGenresCategoriesEntity
  addStatement $ WorkGenreCategory (WorkId filmoId) (GenreCategoryId sujetId)
