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
module Data.CQLOD.Readers.CineTV.Fonction
  ( readFonction
  )
where

import           Data.CQLOD                   (CQLODStatements, addStatement, CQLODStatement (..), Role (..), RoleId(..))
import           Database.CineTv.Public.Model (Fonction (..))
import           Import                       hiding ((^.))
import           Util                         (sqlKeyToText)

import           Data.Pool                    (Pool)
import           Database.Esqueleto           hiding (get)

readFonction :: (MonadIO m) => Pool SqlBackend -> CQLODStatements m ()
readFonction pool = do
  fonctionEntities <-
    liftIO
    $ flip liftSqlPersistMPool pool
    $ select
    $ distinct
    $ from
    $ \fonction -> return fonction
  mapM_ createStatements fonctionEntities

createStatements :: (Monad m) => Entity Fonction -> CQLODStatements m ()
createStatements fonctionEntity = do
  let rid = (RoleId . sqlKeyToText . entityKey) fonctionEntity
  let rname = (fonctionTerme . entityVal) fonctionEntity
  addStatement $ RoleDeclaration $ Role rid rname
