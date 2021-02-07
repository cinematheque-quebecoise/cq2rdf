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
module Data.CQLOD.Readers.CineTV.FilmoPays
  ( readFilmoPays
  )
where

import           Data.CQLOD                   (CQLODStatements, addStatement, CQLODStatement (..),
                                               PlaceId (..), RecordingEventId(..))
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readFilmoPays :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoPays pool = do
  filmo_PaysEntities <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoPays, pays) -> do
        where_
          (   (pays ^. PaysId ==. filmoPays ^. Filmo_PaysPaysId)
          &&. (filmo ^. FilmoId ==. filmoPays ^. Filmo_PaysFilmoId)
          )
        return filmoPays
  mapM_ createStatements filmo_PaysEntities

createStatements :: (Monad m) => Entity Filmo_Pays -> CQLODStatements m ()
createStatements filmo_PaysEntity = do
  let filmoId = sqlKeyToText $ filmo_PaysFilmoId $ entityVal filmo_PaysEntity
  let placeId = sqlKeyToText $ filmo_PaysPaysId $ entityVal filmo_PaysEntity
  addStatement $ RecordingEventLocation (RecordingEventId filmoId) (PlaceId placeId)
