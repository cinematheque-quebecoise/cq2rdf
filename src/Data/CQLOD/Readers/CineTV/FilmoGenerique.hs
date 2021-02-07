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
module Data.CQLOD.Readers.CineTV.FilmoGenerique
  ( readFilmoGenerique
  )
where

import           Data.CQLOD                   (CQLODStatement (..),
                                               CQLODStatements,
                                               LegalBodyId(..), RecordingEventId(..), RecordingActivityId(..),
                                               RoleId(..), PersonId(..), WorkId (..), addStatement)
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readFilmoGenerique
  :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoGenerique pool = do
  filmoGeneriqueEntities <- liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \filmoGenerique -> return filmoGenerique
  mapM_ createStatements filmoGeneriqueEntities

createStatements
  :: (Monad m) => Entity Filmo_Generique -> CQLODStatements m ()
createStatements filmoGeneriqueEntity = do
  let roleId = (sqlKeyToText . filmo_GeneriqueFonctionId . entityVal) filmoGeneriqueEntity
  if roleId == "33" then createDerivingWorkStatements filmoGeneriqueEntity else createRecordingActivityTriples filmoGeneriqueEntity

createDerivingWorkStatements
  :: (Monad m) => Entity Filmo_Generique -> CQLODStatements m ()
createDerivingWorkStatements filmoGeneriqueEntity = do
  let generiqueId = sqlKeyToText $ entityKey filmoGeneriqueEntity
  let filmoId = (sqlKeyToText . filmo_GeneriqueFilmoId . entityVal) filmoGeneriqueEntity
  let nomIdMaybe = sqlKeyToText <$> filmo_GeneriqueNomId (entityVal filmoGeneriqueEntity)
  let organismeIdMaybe = sqlKeyToText <$> filmo_GeneriqueOrganismeId (entityVal filmoGeneriqueEntity)

  forM_ nomIdMaybe $ \nomId -> do
    addStatement $ WorkSourcePerson (WorkId filmoId) (WorkId $ filmoId <> "-" <> generiqueId) (PersonId nomId)

  forM_ organismeIdMaybe $ \organismeId -> do
    addStatement $ WorkSourceLegalBody (WorkId filmoId) (WorkId $ filmoId <> "-" <> generiqueId) (LegalBodyId organismeId)

createRecordingActivityTriples
  :: (Monad m) => Entity Filmo_Generique -> CQLODStatements m ()
createRecordingActivityTriples filmoGeneriqueEntity = do
  let generiqueId = sqlKeyToText $ entityKey filmoGeneriqueEntity
  let filmoId =
        (sqlKeyToText . filmo_GeneriqueFilmoId . entityVal) filmoGeneriqueEntity
  let fonctionId = (sqlKeyToText . filmo_GeneriqueFonctionId . entityVal)
        filmoGeneriqueEntity
  let recordingActivityId = RecordingActivityId generiqueId
  addStatement $ RecordingEventActivity (RecordingEventId filmoId) recordingActivityId

  forM_ (sqlKeyToText <$> filmo_GeneriqueNomId (entityVal filmoGeneriqueEntity)) $ \nomId -> do
    addStatement $ RecordingActivityPersonDeclaration recordingActivityId
                                                      (PersonId nomId)
                                                      (RoleId fonctionId)
  forM_ (sqlKeyToText <$> filmo_GeneriqueOrganismeId (entityVal filmoGeneriqueEntity)) $ \organismeId -> do
    addStatement $ RecordingActivityLegalBodyDeclaration recordingActivityId
                                                         (LegalBodyId organismeId)
                                                         (RoleId fonctionId)

  -- let filmoId = (sqlKeyToText . filmo_GeneriqueFilmoId . entityVal)
  --       filmoRealisationEntity
  -- let nomId = (sqlKeyToText . filmo_GeneriqueNomId . entityVal)
  --       filmoRealisationEntity
  -- let recordingActivityId =
  --       RecordingActivityId $ "Director" <> filmoId <> "-" <> nomId
  -- addStatement
  --   $ RecordingEventActivity (RecordingEventId filmoId) recordingActivityId
  -- addStatement $ RecordingActivityPersonDeclaration recordingActivityId
  --                                                   (PersonId nomId)
  --                                                   (RoleId "1")
