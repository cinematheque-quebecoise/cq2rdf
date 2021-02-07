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
module Data.CQLOD.Readers.CineTV.FilmoRealisation
  ( readFilmoRealisation
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements, PersonId (..),
                                               RecordingActivityId (..),
                                               RecordingEventId (..),
                                               RoleId (..), addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readFilmoRealisation :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoRealisation pool = do
  filmoRealisationEntities <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoRealisation, nom) -> do
        where_
          (   (nom ^. NomId ==. filmoRealisation ^. Filmo_RealisationNomId)
          &&. (   filmo
              ^.  FilmoId
              ==. filmoRealisation
              ^.  Filmo_RealisationFilmoId
              )
          )
        return filmoRealisation

  mapM_ createStatements filmoRealisationEntities

createStatements
  :: (Monad m) => Entity Filmo_Realisation -> CQLODStatements m ()
createStatements filmoRealisationEntity = do
  let filmoId = (sqlKeyToText . filmo_RealisationFilmoId . entityVal)
        filmoRealisationEntity
  let nomId = (sqlKeyToText . filmo_RealisationNomId . entityVal)
        filmoRealisationEntity
  let recordingActivityId =
        RecordingActivityId $ "Director" <> filmoId <> "-" <> nomId
  addStatement
    $ RecordingEventActivity (RecordingEventId filmoId) recordingActivityId
  addStatement $ RecordingActivityPersonDeclaration recordingActivityId
                                                    (PersonId nomId)
                                                    (RoleId "1")
