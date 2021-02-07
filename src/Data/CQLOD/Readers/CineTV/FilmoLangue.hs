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
module Data.CQLOD.Readers.CineTV.FilmoLangue
  ( readFilmoLangue
  )
where

import           Data.CQLOD                   (CQLODStatements, addStatement, CQLODStatement (..),
                                               RecordingId (..), LanguageId(..))
import           Database.CineTv.Public.Model
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readFilmoLangue :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFilmoLangue pool = do
  filmoLangueEntities <- liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \(filmo, filmoLangue, langue) -> do
        where_
          (   (langue ^. LangueId ==. filmoLangue ^. Filmo_LangueLangueId)
          &&. (filmo ^. FilmoId ==. filmoLangue ^. Filmo_LangueFilmoId)
          )
        return filmoLangue
  mapM_ createStatements filmoLangueEntities

createStatements :: (Monad m) => Entity Filmo_Langue -> CQLODStatements m ()
createStatements filmoLangueEntity = do
  let filmoId = sqlKeyToText $ filmo_LangueFilmoId $ entityVal filmoLangueEntity
  let langId = sqlKeyToText $ filmo_LangueLangueId $ entityVal filmoLangueEntity
  addStatement $ RecordingLanguage (RecordingId filmoId) (LanguageId langId)

