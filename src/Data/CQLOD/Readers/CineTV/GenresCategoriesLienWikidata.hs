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
module Data.CQLOD.Readers.CineTV.GenresCategoriesLienWikidata
  ( readGenresCategoriesLienWikidata
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements,
                                               GenreCategoryId (..),
                                               WikidataUri (..), addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readGenresCategoriesLienWikidata
  :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readGenresCategoriesLienWikidata pool = do
  genresCategoriesLienWikidataEntities <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \genresCategories -> return genresCategories
  mapM_ createStatements genresCategoriesLienWikidataEntities

createStatements
  :: (Monad m) => Entity GenresCategories_LienWikidata -> CQLODStatements m ()
createStatements genresCategoriesLienWikidataEntity = do
  let wikidataUriMaybe =
        WikidataUri <$> genresCategories_LienWikidataLienWikidata
          (entityVal genresCategoriesLienWikidataEntity)
  forM_ wikidataUriMaybe $ \wikidataUri -> do
    let genresCategoriesId =
          GenreCategoryId
            $ sqlKeyToText
            $ genresCategories_LienWikidataSujetId
            $ entityVal genresCategoriesLienWikidataEntity
    addStatement $ GenreCategoryWikidataLink genresCategoriesId wikidataUri
